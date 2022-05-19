%%%-------------------------------------------------------------------
%%% @copyright 2018-22 Aeternity Anstalt
%%% @author Ulf Wiger <ulf@wiger.net>
%%% @hidden
%%%-------------------------------------------------------------------
-module(app_ctrl_ctrl).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(st, { app
            , start_requested  = false :: boolean()
            , start_reply_sent = false :: boolean()
            , change_requested = false :: boolean()
            , should_run       = false :: boolean()
            , running          = false :: boolean()
            , loaded           = false :: boolean()
            }).

-define(AC, application_controller).

start_link(App) ->
    gen_server:start_link(?MODULE, App, []).

init(App) ->
    ?LOG_DEBUG("init(~p)", [App]),
    case application_controller:control_application(App) of
        true ->
            {ok, #st{app = App}};
        false ->
            {error, {could_not_control, App}}
    end.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast({Event, App, _Node}, #st{app = MyApp} = St) when Event == app_running;
                                                             Event == app_stopped ->
    case App =/= MyApp of
        true ->
            {noreply, check_app(St)};
        false ->
            {noreply, St}
    end;
handle_cast({new_mode, Mode}, #st{app = MyApp} = St) ->
    ?LOG_DEBUG("New mode (~p): ~p", [MyApp, Mode]),
    {noreply, check_app(St)};
handle_cast(Msg, #st{app = MyApp} = St) ->
    ?LOG_DEBUG("Unknown cast (~p): ~p", [MyApp, Msg]),
    {noreply, St}.

handle_info({ac_load_application_req, App}, #st{app = MyApp} = St) ->
    {Reply, St1} = case App =:= MyApp of
                       true  -> {ok, St#st{loaded = true}};
                       false -> {{error, wrong_app}, St}  % shouldn't happen
                   end,
    ?AC ! {ac_application_load_reply, App, Reply},
    {noreply, St1};
handle_info({ac_start_application_req, App} = Msg, #st{app = MyApp} = St0) ->
    ?LOG_DEBUG("start_application_req: ~p", [App]),
    St = St0#st{start_requested = true},
    case App =:= MyApp of
        true ->
            case ok_to_start(MyApp) of
                true ->
                    ?LOG_DEBUG("will start ~p", [MyApp]),
                    {noreply, instruct_ac(run, St)};
                false ->
                    ?LOG_DEBUG("WON'T start ~p", [MyApp]),
                    {noreply, instruct_ac(dont_run, St)}
            end;
        false ->
            %% Don't use the standard reply helpers, as they note our
            %% own reply state. BTW, this state should never occur unless
            %% something is badly wrong or we're being messed with.
            logger:error("Sent to wrong app controller: ~p", [Msg]),
            send_to_ac({ac_start_application_reply, App, {error, wrong_app}}),
            {noreply, St}
    end;
handle_info({ac_application_run, App, Res}, #st{app = App} = St) ->
    case Res of
        ok         -> app_ctrl_server:running(App, node());
        {error, _} -> ignore
    end,
    {noreply, app_changed(St#st{running = (ok == Res)})};
handle_info({ac_application_not_run, App}, #st{app = App} = St) ->
    app_ctrl_server:stopped(App, node()),
    {noreply, app_changed(St#st{running = false})};
handle_info({ac_application_stopped, App} = M, #st{app = App} = St) ->
    ?LOG_DEBUG("~p", [M]),
    case St#st.running of
        true ->
            %% TODO: this looks weird
            ?AC ! {ac_change_application_req, App, stop_it},
            {noreply, St};
        false ->
            {noreply, St}
    end;
handle_info({ac_application_unloaded, App}, #st{app = App} = St) ->
    {noreply, St#st{loaded = false}};
handle_info(Msg, #st{app = A} = St) ->
    ?LOG_DEBUG("UNKNOWN INFO (~p): ~p", [A, Msg]),
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

ok_to_start(App) ->
    ?LOG_DEBUG("ok_to_start(~p)", [App]),
    %% Res = app_ctrl_server:check_dependencies(Deps),
    Res = app_ctrl_server:ok_to_start(App),
    ?LOG_DEBUG("ok_to_start(~p) -> ~p", [App, Res]),
    Res == true.

ok_to_stop(App) ->
    ?LOG_DEBUG("ok_to_stop(~p)", [App]),
    Res = app_ctrl_server:ok_to_stop(App),
    ?LOG_DEBUG("ok_to_stop(App = ~p) -> ~p", [App, Res]),
    Res == true.

check_app(#st{start_requested = false} = St) ->
    %% Cannot do anything until we get a start request.
    St;
check_app(#st{app = A} = St) ->
    ShouldRun = app_ctrl_server:should_i_run(A),
    app_should_run(ShouldRun, St).

app_changed(St) ->
    maybe_check_app(St#st{change_requested = false}).

maybe_check_app(#st{running = R, should_run = R} = St) ->
    St;
maybe_check_app(St) ->
    check_app(St).

app_should_run(Bool, #st{running = Bool} = St) ->
    St;
app_should_run(false, #st{app = MyApp, running = true} = St) ->
    St1 = case ok_to_stop(MyApp) of
              true ->
                  instruct_ac(dont_run, St);
              false ->
                  St
          end,
    St1#st{should_run = false};
app_should_run(true, #st{app = MyApp, running = false} = St) ->
    St1 = case ok_to_start(MyApp) of
              true ->
                  instruct_ac(run, St);
              false ->
                  St
          end,
    St1#st{should_run = true}.

instruct_ac(_, #st{change_requested = true, app = A} = St) ->
    %% Wait for response from AC, then re-assess
    ?LOG_DEBUG("~p still waiting for AC...", [A]),
    St;
instruct_ac(run     , St) -> tell_ac_(start_it, start_it, St);
instruct_ac(dont_run, St) -> tell_ac_(not_started, stop_it, St).

tell_ac_(_Reply, Change, #st{start_reply_sent = true} = St) ->
    ac_change_req(Change, St),
    St#st{change_requested = true};
tell_ac_(Reply, _Change, St) ->
    ac_start_reply(Reply, St),
    St#st{start_reply_sent = true, change_requested = (Reply == start_it)}.

ac_start_reply(Reply, #st{app = App}) ->
    send_to_ac({ac_start_application_reply, App, Reply}).

ac_change_req(Req, #st{app = App}) ->
    send_to_ac({ac_change_application_req, App, Req}).

send_to_ac(Msg) ->
    ?LOG_DEBUG("AC ! ~p", [Msg]),
    ?AC ! Msg.
