%%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018-21, Aeternity Anstalt
%%% @author Ulf Wiger <ulf@wiger.net>
%%% @hidden
%%%-------------------------------------------------------------------

%%% Dependency management
%%% Note: The idea is to evolve `app_ctrl` into a cluster controller.
%%% Some design decisions are driven by this, and may look out of place
%%% as long as only local application control is supported.
%%%
%%% Such a thing is the wrapping of application identifiers as
%%% `{app_name(), node()}` tuples. In some interactions with the local
%%% application_controller, it is vital to know whether to report a
%%% remote application as running: If an instance of the application in
%%% question is supposed to be running locally, we must NOT report the
%%% event, since it would cause the application_controller to automatically
%%% stop the local instance. The assumption of the application_controller
%%% is that a dist_ac-controlled application runs in one place only, but
%%% `app_ctrl` allows for other configurations, such as multiple mated
%%% pairs, load-sharing pools, etc.
%%%
%%% This module also builds a digraph of all detected app dependencies.
%%% This is currently overkill, but the code is kept, as it may become
%%% handy later on, perhaps partly in an introspection API, where app
%%% dependencies can be conventiently explored.
%%%
-module(app_ctrl_deps).

-export([dependencies/0,
         dependencies/1,
         role_apps/0,
         valid_mode/1,
         apps_in_mode/1
        ]).

-export([get_dependencies_of_app/2,
         get_app_dependencies/2]).

%% -ifdef(TEST).
%% -include_lib("eunit/include/eunit.hrl").
%% -endif.
-include_lib("kernel/include/logger.hrl").

dependencies() ->
    CtrlApps = ctrl_apps(),
    ensure_all_loaded(CtrlApps),
    CtrlApps1 = add_app_deps(app_deps(), CtrlApps),
    dependencies(CtrlApps1).

dependencies(CtrlApps) ->
    G = digraph:new(),
    ?LOG_DEBUG("CtrlApps = ~p~n", [CtrlApps]),
    add_vertices(CtrlApps, G),
    add_edges(start_before, CtrlApps, G),
    add_edges(start_after, CtrlApps, G),
    check_for_cycles(G),
    Deps = [{V, reachable_neighbours([V], G)}
            || V <- vertices(G)],
    #{ graph => G
     , annotated =>  annotate(add_final_deps(Deps, CtrlApps), G) }.

role_apps() ->
    Roles = app_ctrl_config:roles(),
    apps_of_roles(Roles).

valid_mode(Mode) ->
    lists:keymember(Mode, 1, app_ctrl_config:modes()).

apps_in_mode(Mode) ->
    Modes = app_ctrl_config:modes(),
    ?LOG_DEBUG("Modes = ~p", [Modes]),
    case lists:keyfind(Mode, 1, Modes) of
        {_, Roles} ->
            RolesEnv = app_ctrl_config:roles(),
            RoleInfo = [RI || {R, _} = RI <- RolesEnv,
                              lists:member(R, Roles)],
            apps_of_roles(RoleInfo);
        false ->
            ?LOG_ERROR("Invalid app_ctrl mode: ~p", [Mode]),
            error({unknown_mode, Mode})
    end.

get_dependencies_of_app(Graph, App) when is_atom(App) ->
    [A || {_,{A,_},_,_} <- [digraph:edge(Graph, E)
                            || E <- digraph:in_edges(Graph, {App,node()})]].

get_app_dependencies(Graph, App) when is_atom(App) ->
    [A || {_,_,{A,_},_} <- [digraph:edge(Graph, E)
                            || E <- digraph:out_edges(Graph, {App,node()})]].

%% We want to figure out which apps to control. Basically these are
%% all the apps mentioned in 'applications' and 'roles'.
ctrl_apps() ->
    ctrl_apps_(app_ctrl_config:applications()).

ctrl_apps_(L) ->
    RoleApps = [{A, #{}} || A <- role_apps()],
    lists:map(
      fun({A, #{} = M}) ->
              {app_vertex(A), normalize_vertices(M)};
          ({A, Ps}) when is_list(Ps) ->
              {app_vertex(A), normalize_vertices(maps:from_list(Ps))}
      end, RoleApps ++ L).

apps_of_roles(Roles) ->
    remove_duplicates(
      lists:append(
        [As1 || {_Role, As1} <- Roles])).

remove_duplicates(L) ->
    lists:foldr(fun(H, T) -> [H|lists:delete(H, T)] end, [], L).

normalize_vertices(M) ->
    normalize_vertices_(
      start_before, normalize_vertices_(start_after, M)).

normalize_vertices_(K, M) ->
    case maps:find(K, M) of
        {ok, L} ->
            maps:put(K, [app_vertex(A) || A <- L], M);
        error ->
            M
    end.

add_app_deps(ADeps, CtrlApps) ->
    add_deps(start_after, ADeps, CtrlApps).

add_final_deps(Deps, CtrlApps) ->
    add_deps(deps, Deps, CtrlApps).

annotate(Apps, G) ->
    [annotate_(A, G) || A <- Apps].

annotate_({A, #{deps := Deps} = I}, G) ->
    OE = digraph:out_edges(G, A),
    NewDeps = get_labels(OE, Deps, G),
    {A, I#{annotated => NewDeps}}.

get_labels(OutEdges, Deps, G) ->
    case get_labels_(OutEdges, G, {[], Deps}) of
        {Ls, []} ->
            Ls;
        {Ls, [H|T]} ->
            Ls ++ get_labels_(digraph:out_edges(G, H), T, G)
    end.

get_labels_(OutEdges, G, Acc) ->
    lists:foldl(
      fun(_, {_, []} = Acc1) ->
              Acc1;
         (E, {Ls, Ds} = Acc1) ->
              case digraph:edge(G, E) of
                  false ->  % shouldn't happen
                      Acc1;
                  {_,_,To,Label} ->
                      case lists:member(To, Ds) of
                          true ->
                              get_labels_(
                                digraph:out_edges(G, To),
                                G,
                                {[{To, Label}|Ls],
                                 lists:delete(To, Ds)});
                          false ->
                              Acc1
                      end
              end
      end, Acc, OutEdges).

add_deps(K, ADeps, CtrlApps) ->
    lists:foldl(
      fun({A, Deps}, Acc) ->
              case lists:keyfind(A, 1, Acc) of
                  {_, M} ->
                      lists:keyreplace(
                        A, 1, Acc,
                        {A, M#{K => union(Deps, maps:get(K, M, []))}});
                  false ->
                      [{A, #{K => Deps}}|Acc]
              end
      end, CtrlApps, ADeps).

union([], B) -> B;
union(A, []) -> A;
union(A0, B0) ->
    A = lists:usort(A0),
    B = lists:usort(B0),
    (A -- B) ++ (B -- A).

app_deps() ->
    [{app_vertex(A),
      [app_vertex(D) || D <- ok(application:get_key(A, applications))]}
     || {A, _, _} <- application:loaded_applications()].

ok({ok, Res}) ->
    Res;
ok(Other) ->
    erlang:error({unexpected, Other}).

add_vertices(L, G) ->
    lists:foreach(
      fun({A, M}) ->
              add_vertex(G, app_vertex(A)),
              [add_vertex(G, D)
               || D <- maps:get(start_before, M, [])],
              [add_vertex(G, D)
               || D <- maps:get(start_after, M, [])]
      end, L).

app_name(A) when is_atom(A) -> A;
app_name({A, _}) when is_atom(A) -> A.

app_vertex(A) when is_atom(A) ->
    {A, node()};
app_vertex({A,N} = V) when is_atom(A), is_atom(N) ->
    V.

add_edges(K, L, G) ->
    [[add_edge(K, G, app_vertex(B), A, {K, B})
      || B <- maps:get(K, M, [])]
     || {A, M} <- L],
    ok.

check_for_cycles(G) ->
    case [C || C <- [digraph:get_cycle(G, V)
                     || V <- digraph:vertices(G)],
               is_list(C)] of
        [] ->
            ok;
        Cycles ->
            erlang:error({cyclical_dependencies, Cycles})
    end.

ensure_all_loaded(Deps) ->
    All = all_app_names(Deps),
    case not_loaded(All) of
        [_|_] = NotLoaded ->
            try_load_apps(NotLoaded);
        [] -> ok
    end.

all_app_names(Deps) ->
    lists:foldl(
      fun({A,Ds}, Acc) ->
              ordsets:union(
                ordsets:from_list(
                  [app_name(X) || X <- maps:get(start_after, Ds, [])]
                  ++ [app_name(X) || X <- maps:get(start_before, Ds, [])]),
                ordsets:add_element(app_name(A), Acc))
      end, ordsets:new(), Deps).

not_loaded(All) ->
    Loaded = [A || {A,_,_} <- application:loaded_applications()],
    [A || A <- All,
          not lists:member(A, Loaded)].

try_load_apps(Apps) ->
    case lists:foldl(
           fun(A, Acc) ->
                   case application:load(A) of
                       ok ->
                           Acc;
                       {error, {already_loaded,_}} ->
                           Acc;
                       {error, _} ->
                           [A|Acc]
                   end
           end, [], Apps) of
        [] ->
            ok;
        [_|_] = CannotLoad ->
            erlang:error({missing_applications, CannotLoad})
    end.

vertices(G) ->
    digraph:vertices(G).

reachable_neighbours(Vs, G) ->
    digraph_utils:reachable_neighbours(Vs, G).

add_vertex(G, V) ->
    digraph:add_vertex(G, V).

add_edge(start_before, G, A, B, L) when A =/= B ->
    digraph:add_edge(G, A, B, L);
add_edge(start_after,  G, A, B, L) when A =/= B ->
    digraph:add_edge(G, B, A, L).
