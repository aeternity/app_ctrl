-module(app_ctrl_event_sup).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], [{debug, [trace]}]).

init([]) ->
    ok = app_ctrl_event:install(),
    {ok, []}.

handle_info({gen_event_EXIT, _Handler, _Reason} = Exit, St) ->
    {stop, Exit, St};
handle_info(_Msg, St) ->
    {noreply, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

