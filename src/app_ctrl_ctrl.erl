%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(app_ctrl_ctrl).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(st, {app,
             start_requested = false,
             start_reply_sent = false,
             running = false,
             loaded  = false,
             deps}).

-define(AC, application_controller).

start_link(App, Deps) ->
    gen_server:start_link(?MODULE, {App, Deps}, []).

init({App, Deps}) ->
    case application_controller:control_application(App) of
        true ->
            {ok, #st{app = App,
                     deps = Deps}};
        false ->
            {error, {could_not_control, App}}
    end.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast({app_running, App, _Node}, #st{app = MyApp} = St) ->
    case App =/= MyApp
        andalso St#st.running           =:= false
        andalso St#st.start_requested   =:= true
        andalso ok_to_start(St) of
        true ->
            case St#st.start_reply_sent of
                false ->
                    ?AC ! {ac_start_application_reply, MyApp, start_it},
                    {noreply, St#st{start_reply_sent = true}};
                true ->
                    ?AC ! {ac_change_application_req, MyApp, start_it},
                    {noreply, St}
            end;
        false ->
            {noreply, St}
    end;
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({ac_load_application_req, App}, #st{app = MyApp} = St) ->
    {Reply, St1} = case App =:= MyApp of
                       true  -> {ok, St#st{loaded = true}};
                       false -> {{error, wrong_app}, St}  % shouldn't happen
                   end,
    ?AC ! {ac_application_load_reply, App, Reply},
    {noreply, St1};
handle_info({ac_start_application_req, App}, #st{app = MyApp} = St) ->
    case App =:= MyApp of
        true ->
            case ok_to_start(St) of
                true ->
                    ?AC ! {ac_start_application_reply, App, start_it},
                    {noreply, St#st{start_requested = true,
                                    start_reply_sent = true}};
                false ->
                    {noreply, St#st{start_requested = true,
                                    start_reply_sent = false}}
            end;
        false ->
            ?AC ! {ac_start_application_reply, App, {error, wrong_app}},
            {noreply, St}
    end;
handle_info({ac_application_run, App, Res}, #st{app = App} = St) ->
    case Res of
        ok         -> app_ctrl_server:running(App, node());
        {error, _} -> ignore
    end,
    {noreply, St#st{running = (ok == Res)}};
handle_info({ac_application_not_run, App, Res}, #st{app = App} = St) ->
    app_ctrl_server:stopped(App, node()),
    {noreply, St#st{running = (ok =/= Res) orelse St#st.running}};
handle_info({ac_application_stopped, App}, #st{app = App} = St) ->
    case St#st.running of
        true ->
            ?AC ! {ac_change_application_req, App, stop_it},
            {noreply, St};
        false ->
            {noreply, St}
    end;
handle_info({ac_application_unloaded, App}, #st{app = App} = St) ->
    {noreply, St#st{loaded = false}};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

ok_to_start(#st{deps = #{deps := Deps}}) ->
    app_ctrl_server:check_dependencies(Deps).
