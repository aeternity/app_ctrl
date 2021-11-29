%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(app_ctrl_server_proxy).
-behaviour(gen_server).

%%% This module exists to enable supervision by proxy on the
%%% app_ctrl_server, which is started early during kernel app startup.
%%% Its mission is only to link to the app_ctrl_server, so that it dies
%%% when the server dies, and vice versa.

-export([ start_link/0 ]).

-export([
          init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-include_lib("kernel/include/logger.hrl").

-record(st, {server :: pid() }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Pid = whereis_server(),
    link(Pid),
    {ok, #st{server = Pid}}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

%% ======================================================================

-spec whereis_server() -> pid().
whereis_server() ->
    case app_ctrl_server:whereis() of
        undefined ->
            ?LOG_DEBUG("app_ctrl_server:whereis() -> undefined", []),
            case logger:get_handler_config(app_ctrl) of
                {ok, #{config := #{app_ctrl_server := Pid}}} ->
                    ?LOG_DEBUG("got pid from handler config: ~p", [Pid]),
                    Pid;
                {error, {not_found,_}} ->
                    ?LOG_DEBUG("handler not installed, installing", []),
                    ok = logger:add_handler(app_ctrl, app_ctrl_bootstrap, #{}),
                    {ok, #{config := #{app_ctrl_server := Pid}}} =
                        logger:get_handler_config(app_ctrl),
                    ?LOG_DEBUG("handler installed Pid = ~p", [Pid]),
                    Pid
            end;
        Pid ->
            ?LOG_DEBUG("app_ctrl_server Pid = ~p", [Pid]),
            Pid
    end.
