%%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
%% @doc app_ctrl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(app_ctrl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [?CHILD(app_ctrl_server_proxy, 5000, worker)]} }.

%%====================================================================
%% Internal functions
%%====================================================================
