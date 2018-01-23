%%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(app_ctrl_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([running/2,
         stopped/2,
         check_dependencies/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(st, {controllers = []}).
-define(TAB, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

running(App, Node) ->
    gen_server:cast(?MODULE, {app_running, App, Node}).

stopped(App, Node) ->
    gen_server:cast(?MODULE, {app_stopped, App, Node}).

check_dependencies(Deps) ->
    gen_server:call(?MODULE, {check, Deps}).

init([]) ->
    ets:new(?TAB, [ordered_set, named_table]),
    ets:insert(?TAB, [{{A, node()}}
                      || {A, _, _} <- application:which_applications()]),
    Controllers = start_controllers(),
    permit_applications(Controllers),
    {ok, #st{controllers = Controllers}}.


handle_call({check, Deps}, _From, #st{} = St) ->
    {reply, check_(Deps), St};
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

is_member(A, #{deps := Deps}) ->
    lists:member(A, Deps).

start_controllers() ->
    [start_controller(A, Deps)
     || {{A,N}, Deps} <- app_ctrl_deps:dependencies(),
        N =:= node(),
        not lists:member(A, [kernel, stdlib, app_ctrl])].

tell_controllers(Event, App, Node, #st{controllers = Cs}) ->
    [gen_server:cast(Pid, {Event, App, Node})
     || {A, Pid, Deps} <- Cs,
        A =/= App,
        is_member({App,Node}, Deps)],
    ok.

start_controller(A, Deps) ->
    {ok, Pid} = app_ctrl_ctrl:start_link(A, Deps),
    {A, Pid, Deps}.

permit_applications(Controllers) ->
    AllApps = ordsets:union(
                [
                 static_permissions(),
                 ordsets:from_list(
                   [A || {A,_,_} <- Controllers])
                 | [ordsets:from_list(
                      [A || {A,N} <- maps:get(deps, M, []),
                            N =:= node()])
                    || {_, _, M} <- Controllers]]),
    [application_controller:permit_application(A, true) || A <- AllApps],
    ok.

static_permissions() ->
    ordsets:from_list(
      [A || {A, false} <- application:get_env(kernel, permissions, [])]).

check_(Deps) ->
    lists:all(
      fun({App, N}) ->
              is_atom(App)
                  andalso is_atom(N)
                  andalso ets:member(?TAB, {App, N})
      end, Deps).
