%%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%%
%%% @copyright 2018-22 Aeternity Anstalt
%%% @author Ulf Wiger <ulf@wiger.net>
%%% @doc API module for app_ctrl
%%%
%%% This module provides the main user interface to app_ctrl.
%%% @end
-module(app_ctrl).

-export([ status/0
        , dependencies_of/1
        , app_dependencies/1
        , get_mode/0
        , set_mode/1
        , set_and_await_mode/2
        , is_mode_stable/0
        , await_stable_mode/1
        , check_for_new_applications/0 ]).

-type app()  :: atom().
-type mode() :: atom().
-type role() :: atom().
-type app_status() :: running
                    | ok_to_start
                    | load_only.

-type status() :: #{ current_mode     := mode()
                   , current_roles    := [role()]
                   , running_locally  := [{app(), [node()]}]
                   , running_remotely := [{app(), [node()]}]
                   , allowed_apps     := [app()]
                   , role_apps        := [{app(), app_status()}]
                   }.

-export_type([ app/0
             , mode/0
             , role/0
             , app_status/0
             , status/0
             ]).

-spec status() -> status().
%% @doc Returns information about the current state of the system.
status() ->
    app_ctrl_server:status().

-spec app_dependencies(App) -> [app()]
              when App :: app().
%% @doc Lists the applications on which `App' depends.
app_dependencies(App) ->
    G = app_ctrl_server:graph(),
    app_ctrl_deps:get_app_dependencies(G, App).

-spec dependencies_of(App) -> [app()]
              when App :: app().
%% @doc List all applications that depend on `App'
%% (including `start_before' dependencies.
%% @end
dependencies_of(App) ->
    G = app_ctrl_server:graph(),
    app_ctrl_deps:get_dependencies_of_app(G, App).

-spec get_mode() -> mode().
%% @doc Get the current mode.
get_mode() ->
    app_ctrl_server:get_mode().

-spec set_mode(mode()) -> ok.
%% @doc Sets the current mode to `Mode'.
%% Will raise an exception if `Mode' is not a known mode.
%% @end
set_mode(Mode) ->
    case lists:keymember(Mode, 1, app_ctrl_config:modes()) of
        true ->
            ok = app_ctrl_server:set_mode(Mode);
        false ->
            error(unknown_mode)
    end.

-spec set_and_await_mode(Mode, Timeout) -> {ok, Mode} | {timeout, [app()]}
              when Mode :: mode(),
                   Timeout :: non_neg_integer() | infinity.
%% @doc Like {@link set_mode/1}, but waits until the transition to the
%% new mode has been completed.
%% @end
set_and_await_mode(Mode, Timeout) ->
    set_mode(Mode),
    await_stable_mode(Timeout).

-spec is_mode_stable() -> boolean().
%% @doc Checks whether the system is in a stable mode (`true'), or
%% in transition between modes (`false').
%% @end
is_mode_stable() ->
    app_ctrl_server:is_mode_stable().

-spec await_stable_mode(Timeout) -> {ok, mode()} | {timeout, [app()]}
              when Timeout :: non_neg_integer() | infinity.
%% @doc Waits for the system to become stable (no ongoing mode transition)
%% and returns the active mode, or timeout, including which apps are still
%% pending.
%% @end
await_stable_mode(Timeout) when Timeout == infinity
                              ; is_integer(Timeout), Timeout > 0 ->
    app_ctrl_server:await_stable_mode(Timeout).

-spec check_for_new_applications() -> ok.
%% @doc Checks if applications have been added (or removed) and takes
%% appropriate action.
%% @end.
check_for_new_applications() ->
    app_ctrl_server:check_for_new_applications().
