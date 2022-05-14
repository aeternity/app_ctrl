-module(app_ctrl_events).

-export([ subscribe/1
        , unsubscribe/1
        , publish/2 ]).

-export_type([ event/0
	     , info/0]).

-type event() :: app_running  %% {App, Node}
	       | app_stopped  %% 
	       | new_mode.

-type info() :: app_info()
	      | mode_info().

-type app_info()  :: {app_name(), node()}.
-type mode_info() :: atom().
-type app_name()  :: atom().

-define(PS_TAG, app_ctrl).

-spec subscribe(event()) -> true.
subscribe(EventTag) ->
    gproc_ps:subscribe(l, {?PS_TAG, EventTag}).

-spec unsubscribe(event()) -> true.
unsubscribe(EventTag) ->
    gproc_ps:unsubscribe(l, {?PS_TAG, EventTag}).

-spec publish(event(), info()) -> ok.
publish(EventTag, Info) ->
    gproc_ps:publish(l, {?PS_TAG, EventTag}, Info).
