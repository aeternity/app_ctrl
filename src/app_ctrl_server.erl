%%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(app_ctrl_server).

-behaviour(gen_server).

-export([ start/0
        , start_link/0]).
-export([ status/0
        , running/2
        , stopped/2
        , get_mode/0
        , set_mode/1
        , should_i_run/1
        , ok_to_start/1
        , ok_to_stop/1
        , check_dependencies/1
        , is_mode_stable/0
        , await_stable_mode/1
        , check_for_new_applications/0 ]).

-export([whereis/0]).

-export([graph/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(st, { controllers = []
            , role_apps = []
            , allowed_apps = []
            , other_apps = []
            , live_map = #{}
            , running_where = #{}
            , pending_stable = []
            , mode
            , stable = false
            , graph}).

-define(PROTECTED_MODE, app_ctrl_protected).  %% hard-coded app list

-include_lib("kernel/include/logger.hrl").

%% This is used when the server is bootstrapped from the logger handler,
%% since the handler is initialized from a temporary process.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

status() ->
    gen_server:call(?MODULE, status).

whereis() ->
    whereis(?MODULE).

running(App, Node) ->
    gen_server:cast(?MODULE, {app_running, App, Node}).

stopped(App, Node) ->
    gen_server:cast(?MODULE, {app_stopped, App, Node}).

get_mode() ->
    gen_server:call(?MODULE, get_mode).

set_mode(Mode) ->
    case gen_server:call(?MODULE, {set_mode, Mode}) of
        ok ->
            app_ctrl_config:set_current_mode(Mode),
            ok;
        Error ->
            Error
    end.

check_dependencies(Deps) ->
    gen_server:call(?MODULE, {check, Deps}).

should_i_run(App) ->
    gen_server:call(?MODULE, {should_i_run, App}).

ok_to_start(App) ->
    gen_server:call(?MODULE, {ok_to_start, App}).

ok_to_stop(App) ->
    gen_server:call(?MODULE, {ok_to_stop, App}).

is_mode_stable() ->
    gen_server:call(?MODULE, is_mode_stable).

await_stable_mode(Timeout) ->
    %% We don't want the call itself to time out.
    %% Assume that the server will never get stuck.
    gen_server:call(?MODULE, {await_stable_mode, Timeout}, infinity).

check_for_new_applications() ->
    gen_server:call(?MODULE, check_for_new_applications).

graph() ->
    gen_server:call(?MODULE, graph).

init([]) ->
    St = init_state(#st{}),
    {ok, set_current_mode(?PROTECTED_MODE, St)}.

init_state(#st{} = St0) ->
    case St0#st.graph of
        undefined -> ok;
        G0 -> digraph:delete(G0)
    end,
    #{graph := G, annotated := Deps} = app_ctrl_deps:dependencies(),
    RoleApps = app_ctrl_deps:role_apps(),   %% all apps controlled via roles
    ?LOG_DEBUG(#{role_apps => RoleApps}),
    RoleAppDeps = deps_of_apps(RoleApps, G),
    case [A || A <- RoleAppDeps,
               not lists:member(A, RoleApps)] of
        [] ->
            %% For now, assume that apps are never removed from the system
            Controllers = start_new_controllers(Deps, St0#st.controllers),
            ?LOG_DEBUG("Controllers = ~p", [lists:sort(Controllers)]),
            OtherApps = [A || {A,_} <- Controllers, not lists:member(A, RoleApps)],
            St0#st{ controllers = Controllers
                  , role_apps = RoleApps
                  , other_apps = OtherApps
                  , graph = G};
        Orphans ->
            ?LOG_ERROR(#{apps_not_found => Orphans}),
            error({orphans, Orphans})
    end.


deps_of_apps(As, G) ->
    lists:foldl(
      fun(A, Acc) ->
              lists:foldl(fun ordsets:add_element/2, Acc,
                          app_ctrl_deps:get_dependencies_of_app(G, A))
      end, ordsets:new(), As).

handle_call({check, Deps}, _From, #st{} = St) ->
    {reply, check_(Deps, St), St};
handle_call(get_mode, _From, #st{mode = CurMode} = St) ->
    {reply, CurMode, St};
handle_call({set_mode, Mode}, _From, #st{mode = CurMode} = St) ->
    ?LOG_DEBUG("set_mode: ~p (CurMode = ~p)", [Mode, CurMode]),
    case Mode of
        CurMode ->
            {reply, ok, St};
        _ ->
            case app_ctrl_deps:valid_mode(Mode) of
                true ->
                    St1 = set_current_mode(Mode, St),
                    announce_new_mode(Mode, St1),
                    {reply, ok, St1};
                false ->
                    {reply, {error, unknown_mode}, St}
            end
    end;
handle_call({should_i_run, App}, _From, #st{ allowed_apps = Allowed
                                           , other_apps = Other } = St) ->
    {reply, (lists:member(App, Allowed)
             orelse lists:member(App, Other)), St};
handle_call({ok_to_start, App}, _From, #st{mode = M} = St) ->
    ?LOG_DEBUG("ok_to_start ~p, mode = ~p", [App, M]),
    {reply, ok_to_start_(App, St), St};
handle_call({ok_to_stop, App}, _From, #st{graph = G} = St) ->
    case [A || {_,A,App1,{start_after,App1}} <- in_edges(G, App),
               App1 =:= App,
               is_running(A, St)] of
        [] ->
            {reply, true, St};
        [_|_] = Other ->
            {reply, {false, [{A, still_running} || A <- Other]}, St}
    end;
handle_call(graph, _From, #st{graph = G} = St) ->
    {reply, G, St};
handle_call(status, _From, St) ->
    {reply, status_(St), St};
handle_call(is_mode_stable, _From, St) ->
    {reply, is_stable(St), St};
handle_call({await_stable_mode, Timeout}, From, #st{pending_stable = Pending} = St) ->
    case St#st.stable of
        true ->
            {reply, {ok, St#st.mode}, St};
        false ->
            TRef = set_timeout(Timeout, {awaiting_stable, From}),
            {noreply, St#st{pending_stable = [{From, TRef} | Pending]}}
    end;
handle_call(check_for_new_applications, _From, #st{ mode = Mode
                                                  , allowed_apps = Allowed0} = St0) ->
    St = init_state(St0),
    case allowed_apps(St#st.mode, St) of
        Allowed0 ->
            {reply, ok, St};
        NewAllowedApps ->
            St1 = St#st{allowed_apps = NewAllowedApps},
            case St1#st.stable of
                false ->
                    {reply, ok, St1};
                true ->
                    announce_new_mode(Mode, St1),
                    {reply, ok, St1#st{stable = false}}
            end
    end;
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast({app_running, App, Node}, #st{mode = Mode} = St0) ->
    St = note_running({App, Node}, true, St0),
    tell_controllers(app_running, App, Node, St),
    publish_event(app_running, {App, Node}, St),
    case {Mode, App} of
        {?PROTECTED_MODE, app_ctrl} ->
            ?LOG_DEBUG("Moving to default mode", []),
            {noreply, set_next_mode(St)};
        _ ->
            {noreply, check_if_stable(St)}
    end;
handle_cast({app_stopped, App, Node}, #st{} = St0) ->
    St = note_running({App, Node}, false, St0),
    tell_controllers(app_stopped, App, Node, St),
    publish_event(app_stopped, {App, Node}, St),
    {noreply, check_if_stable(St)};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({timeout, TRef, {awaiting_stable, From}}, #st{pending_stable = Pend} = St) ->
    case lists:keytake(TRef, 2, Pend) of
        {value, {From, _}, Pend1} ->
            case is_stable(St) of
                true ->
                    notify_pending({ok, St#st.mode}, St#st{stable = true});
                {false, Outstanding} ->
                    gen_server:reply(From, {timeout, Outstanding}),
                    {noreply, St#st{pending_stable = Pend1}}
            end;
        false ->
            {noreply, St}
    end;
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

start_new_controllers(AnnotatedDeps, Controllers) ->
    [ start_controller(A)
      || {{A,N} = AppNode, _Deps} <- AnnotatedDeps,
         N =:= node(),
         not lists:member(A, [kernel, stdlib])
             andalso not lists:keymember(A, 1, Controllers)
             andalso not load_only(AppNode) ]
        ++ Controllers.

announce_new_mode(Mode, St) ->
    tell_controllers({new_mode, Mode}, St),
    publish_event(new_mode, Mode, St).

tell_controllers(Event, App, Node, #st{controllers = Cs}) ->
    ?LOG_DEBUG("tell_controllers(~p, ~p, ~p)", [Event, App, Node]),
    [gen_server:cast(Pid, {Event, App, Node}) || {_, Pid} <- Cs],
    ok.

tell_controllers(Event, #st{controllers = Cs}) ->
    ?LOG_DEBUG("tell_controllers(~p)", [Event]),
    [gen_server:cast(Pid, Event)
     || {_, Pid} <- Cs],
    ok.

start_controller(A) ->
    {ok, Pid} = app_ctrl_ctrl:start_link(A),
    {A, Pid}.

note_running(A, Bool, #st{live_map = Map, running_where = Where} = St) when is_boolean(Bool) ->
    {AppName, Node} = A,
    Map1 =
        maps:update_with(A, fun(M0) ->
                                    M = maps:without([ongoing], M0),
                                    M#{running => Bool}
                            end, #{running => Bool}, Map),
    Where1 = case Bool of
                 true ->
                     maps:update_with(AppName,
                                      fun(Ns) ->
                                              ordsets:add_element(Node, Ns)
                                      end, ordsets:from_list([Node]), Where);
                 false ->
                     maps:update_with(AppName,
                                      fun(Ns) ->
                                              ordsets:del_element(Node, Ns)
                                      end, ordsets:new(), Where)
             end,
    St#st{live_map = Map1, running_where = Where1}.

check_(Deps, St) ->
    case check_(Deps, St, []) of
        [] ->
            true;
        Other ->
            {false, Other}
    end.

check_([{App, N} = A|Deps], St, Acc) when is_atom(App), N == node() ->
    case (is_running(A, St) orelse load_only(A)) of
        true  -> check_(Deps, St, Acc);
        false -> check_(Deps, St, [{App, not_running}|Acc])
    end;
check_([], _St, Acc) ->
    Acc.

load_only({App, N}) when N == node() ->
    case application:get_key(App, mod) of
        {ok, {_, _}} -> false;
        {ok, []}     -> true;
        undefined    -> true
    end.

is_running({_App,N} = A, #st{live_map = Map}) when N == node() ->
    case maps:find(A, Map) of
        {ok, #{running := Bool}} -> Bool;
        error -> false
    end.

set_timeout(infinity, _) ->
    infinity;
set_timeout(T, Msg) when is_integer(T), T >= 0 ->
    erlang:start_timer(T, self(), Msg).


check_if_stable(#st{mode = Mode} = St) ->
    case is_stable(St) of
        true ->
            notify_pending({ok, Mode}, St#st{stable = true});
        {false, _Outstanding} ->
            St
    end.

is_stable(#st{} = St) ->
    case not_running(St) ++ not_stopped(St) of
        [] ->
            true;
        Other ->
            {false, Other}
    end.

not_running(#st{allowed_apps = Allowed} = St) ->
    opt_attr(not_running, [A || A <- Allowed,
                                not is_running({A, node()}, St)]).

not_stopped(#st{allowed_apps = Allowed, role_apps = RApps} = St) ->
    opt_attr(not_stopped, [R || R <- (RApps -- Allowed),
                                is_running({R, node()}, St)]).

opt_attr(_  , []) -> [];
opt_attr(Key, L)  -> [{Key, L}].

notify_pending(Msg, #st{pending_stable = Pend} = St) ->
    lists:foreach(
      fun({From, TRef}) ->
              erlang:cancel_timer(TRef),
              gen_server:reply(From, Msg)
      end, Pend),
    St#st{pending_stable = []}.

publish_event(_, _, #st{mode = ?PROTECTED_MODE}) ->
    ok;
publish_event(Event, Info, _St) ->
    app_ctrl_events:publish(Event, Info).

in_edges(G, App) ->
    [digraph:edge(G, E) || E <- digraph:in_edges(G, App)].

out_edges(G, App) ->
    [digraph:edge(G, E) || E <- digraph:out_edges(G, App)].

allowed_apps(undefined, #st{controllers = Cs}) ->
    lists:sort([A || {A, _} <- Cs]);
allowed_apps(?PROTECTED_MODE, #st{controllers = Cs} = St) ->
    any_active(protected_mode_apps(St), Cs);
allowed_apps(Mode, #st{role_apps = _RApps, controllers = _Cs}) ->
    _AppsInMode = lists:sort(app_ctrl_deps:apps_in_mode(Mode)).
    %% OtherControlled = [A || {A,_} <- Cs,
    %%                         not lists:member(A, RApps)],
    %% AppsInMode ++ [A || A <- OtherControlled,
    %%                     not lists:member(A, AppsInMode)].

set_next_mode(St) ->
    Mode = app_ctrl_config:current_mode(),
    St1 = set_current_mode(Mode, St),
    announce_new_mode(Mode, St1),
    St1.

set_current_mode(Mode, #st{} = St) ->
    Allowed = allowed_apps(Mode, St),
    St#st{mode = Mode, allowed_apps = Allowed, stable = false}.

any_active(As, Cs) ->
    intersection(As, [A || {A, _} <- Cs]).

protected_mode_apps(#st{graph = G}) ->
    Res = app_ctrl_config:protected_mode_apps(),
    ?LOG_DEBUG("protected_mode_apps = ~p", [Res]),
    Expanded = add_deps(Res, G),
    ?LOG_DEBUG("Expanded = ~p", [Expanded]),
    Expanded.

add_deps(Apps, G) ->
    lists:foldr(fun(A, Acc) ->
                        add_deps_(A, G, Acc)
                end, Apps, Apps).

add_deps_(A, G, Acc) ->
    Deps = app_ctrl_deps:get_app_dependencies(G, A),
    case Deps -- Acc of
        [] ->
            Acc;
        New ->
            lists:foldr(fun(A1, Acc1) ->
                                add_deps_(A1, G, Acc1)
                        end, add_to_ordset(New, Acc), New)
    end.

add_to_ordset(Xs, Set) ->
    lists:foldr(fun ordsets:add_element/2, Set, Xs).
              

intersection(A, B) ->
    A -- (A -- B).

status_(#st{mode = Mode, allowed_apps = AApps, role_apps = RApps} = St) ->
    Modes = app_ctrl_config:modes(),
    Roles = app_ctrl_config:roles(),
    #{ current_mode => Mode
     , current_roles => proplists:get_value(Mode, app_ctrl_config:modes())
     , running_locally => running_locally(St)
     , running_remotely => running_remotely(St)
     , allowed_apps => [A1 || {_, Status} = A1
                                  <- [ {A, runnable_app_status(A, St)} || A <- AApps ],
                              Status =/= load_only]
     , role_apps    => [ {A, controlled_app_status(A, Modes, Roles)} || A <- RApps] }.

running_locally(#st{running_where = R}) ->
    maps:filter(
      fun(_App, Ns) -> ordsets:is_element(node(), Ns) end, R).

running_remotely(#st{running_where = R}) ->
    maps:filter(
      fun(_App, Ns) -> not ordsets:is_empty(Ns)
                           andalso
                           not ordsets:is_element(node(), Ns)
      end, R).

runnable_app_status(A, St) ->
    ?LOG_DEBUG("runnable_app_status(~p, St)", [A]),
    AppNode = {A, node()},  %% TODO: fix this when adding distributed ctrl
    case is_running(AppNode, St) of
        true ->
            running;
        false ->
            case load_only(AppNode) of
                true ->
                    load_only;
                false ->
                    case ok_to_start_(A, St) of
                        true ->
                            ok_to_start;
                        {false, Why} ->
                            {not_started, Why}
                    end
            end
    end.

controlled_app_status(A, Modes, Roles) ->
    InRoles = [R || {R, As} <- Roles,
                    lists:member(A, As)],
    InModes = [M || {M, Rs} <- Modes,
                    intersection(InRoles, Rs) =/= []],
    #{ in_roles => InRoles
     , in_modes => InModes }.

ok_to_start_(App, #st{allowed_apps = Allowed, other_apps = Other, graph = G} = St) ->
    case lists:member(App, Allowed) orelse lists:member(App, Other) of
        true ->
            case [A || {_,_,A,{start_after,_}} <- out_edges(G, App),
                       not (is_running(A, St) orelse load_only(A))] of
                [] ->
                    true;
                [_|_] = Other ->
                    {false, [{A, not_running} || A <- Other]}
            end;
        false ->
            {false, not_allowed}
    end.
