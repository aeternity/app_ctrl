-module(app_ctrl_event).
-behaviour(gen_event).

-export([install/0]).
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

install() ->
    gen_event:add_sup_handler(error_logger, ?MODULE, []).

init([]) ->
    [app_ctrl_server:running(A, node())
     || {A,_,_} <- application:which_applications()],
    {ok, []}.

handle_event({info_report, _, {_,_,[{application,Name},
                                    {started_at, Node}]}}, St) ->
    app_ctrl_server:running(Name, Node),
    {ok, St};
handle_event(_E, State) ->
    {ok, State}.

handle_call(_Req, State) ->
    {ok, {error, unknown_call}, State}.

handle_info(_Msg, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.
