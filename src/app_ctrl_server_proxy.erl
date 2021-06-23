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

-record(st, {server :: pid() }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case app_ctrl_server:whereis() of
        undefined ->
            {error, no_app_ctrl_server};
        Pid when is_pid(Pid) ->
            link(Pid),
            {ok, #st{server = Pid}}
    end.

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
