%%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(app_ctrl_server).

-behaviour(gen_server).

-export([ start/0
        , start_link/0]).
-export([running/2,
         stopped/2,
         set_mode/1,
         should_i_run/1,
         ok_to_start/1,
         ok_to_stop/1,
         check_dependencies/1]).

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
            , mode
            , graph}).
-define(TAB, ?MODULE).

%% This is used when the server is bootstrapped from the logger handler,
%% since the handler is initialized from a temporary process.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

whereis() ->
    whereis(?MODULE).

running(App, Node) ->
    gen_server:cast(?MODULE, {app_running, App, Node}).

stopped(App, Node) ->
    gen_server:cast(?MODULE, {app_stopped, App, Node}).

set_mode(Mode) ->
    gen_server:call(?MODULE, {set_mode, Mode}).

check_dependencies(Deps) ->
    gen_server:call(?MODULE, {check, Deps}).

should_i_run(App) ->
    gen_server:call(?MODULE, {should_i_run, App}).

ok_to_start(App) ->
    gen_server:call(?MODULE, {ok_to_start, App}).

ok_to_stop(App) ->
    gen_server:call(?MODULE, {ok_to_stop, App}).

graph() ->
    gen_server:call(?MODULE, graph).

init([]) ->
    ets:new(?TAB, [ordered_set, named_table]),
    ets:insert(?TAB, [{{A, node()}}
                      || {A, _, _} <- application:which_applications()]),
    #{graph := G, annotated := Deps} = app_ctrl_deps:dependencies(),
    RoleApps = app_ctrl_deps:role_apps(),   %% all apps controlled via roles
    Controllers = start_controllers(Deps),
    OtherApps = [A || {A,_} <- Controllers, not lists:member(A, RoleApps)],
    Mode = application:get_env(app_ctrl, default_mode, undefined),
    {ok, set_current_mode(
           Mode,
           #st{ controllers = Controllers
              , role_apps = RoleApps
              , other_apps = OtherApps
              , graph = G})}.


handle_call({check, Deps}, _From, #st{} = St) ->
    {reply, check_(Deps), St};
handle_call({set_mode, Mode}, _From, #st{mode = CurMode} = St) ->
    case Mode of
        CurMode ->
            {reply, ok, St};
        _ ->
            case app_ctrl_deps:valid_mode(Mode) of
                true ->
                    St1 = set_current_mode(Mode, St),
                    tell_controllers(new_mode, Mode, node(), St1),
                    {reply, ok, St1};
                false ->
                    {reply, {error, unknown_mode}, St}
            end
    end;
handle_call({should_i_run, App}, _From, #st{allowed_apps = Allowed} = St) ->
    {reply, lists:member(App, Allowed), St};
handle_call({ok_to_start, App}, _From, #st{graph = G} = St) ->
    case [A || {_,_,A,{start_after,_}} <- out_edges(G, App),
               not (running(A) orelse load_only(A))] of
        [] ->
            {reply, true, St};
        [_|_] = Other ->
            {reply, {false, [{A, not_running} || A <- Other]}, St}
    end;
handle_call({ok_to_stop, App}, _From, #st{graph = G} = St) ->
    case [A || {_,A,App1,{start_after,App1}} <- in_edges(G, App),
               App1 =:= App,
               running(A)] of
        [] ->
            {reply, true, St};
        [_|_] = Other ->
            {reply, {false, [{A, still_running} || A <- Other]}, St}
    end;
handle_call(graph, _From, #st{graph = G} = St) ->
    {reply, G, St};
handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast({app_running, App, Node}, #st{} = St) ->
    ets:insert(?TAB, {{App,Node}}),
    tell_controllers(app_running, App, Node, St),
    {noreply, St};
handle_cast({app_stopped, App, Node}, #st{} = St) ->
    ets:delete(?TAB, {App,Node}),
    tell_controllers(app_stopped, App, Node, St),
    {noreply, St};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

start_controllers(AnnotatedDeps) ->
    [start_controller(A)
     || {{A,N}, _Deps} <- AnnotatedDeps,
        N =:= node(),
        not lists:member(A, [kernel, stdlib, app_ctrl])].

tell_controllers(Event, Apps, Node, #st{controllers = Cs}) when is_list(Apps) ->
    [gen_server:cast(Pid, {Event, Apps, Node})
     || {_A, Pid} <- Cs];
tell_controllers(Event, App, Node, #st{controllers = Cs}) ->
    [gen_server:cast(Pid, {Event, App, Node})
     || {A, Pid} <- Cs,
        A =/= App],
    ok.

start_controller(A) ->
    {ok, Pid} = app_ctrl_ctrl:start_link(A),
    {A, Pid}.


check_(Deps) ->
    case check_(Deps, []) of
        [] ->
            true;
        Other ->
            {false, Other}
    end.

check_([{App, N} = A|Deps], Acc) when is_atom(App), N == node() ->
    case (running(A) orelse load_only(A)) of
        true  -> check_(Deps, Acc);
        false -> check_(Deps, [{App, not_running}|Acc])
    end;
check_([], Acc) ->
    Acc.

load_only({App, N}) when N == node() ->
    case application:get_key(App, mod) of
        {ok, {_, _}} -> true;
        {ok, []}     -> false
    end.

running({_App,N} = A) when N == node() ->
    ets:member(?TAB, A).
                         
in_edges(G, App) ->
    [digraph:edge(G, E) || E <- digraph:in_edges(G, App)].

out_edges(G, App) ->
    [digraph:edge(G, E) || E <- digraph:out_edges(G, App)].

allowed_apps(undefined, #st{controllers = Cs}) ->
    [A || {A, _} <- Cs];
allowed_apps(Mode, #st{role_apps = RApps, controllers = Cs}) ->
    AppsInMode = app_ctrl_deps:apps_in_mode(Mode),
    OtherControlled = [A || {A,_} <- Cs,
                            not lists:member(A, RApps)],
    AppsInMode ++ [A || A <- OtherControlled,
                        not lists:member(A, AppsInMode)].

set_current_mode(Mode, #st{other_apps = Other} = St) ->
    Allowed = allowed_apps(Mode, St),
    St#st{mode = Mode, allowed_apps = Allowed ++ Other}.
