-module(app_ctrl_events).

-export([ subscribe/1
        , unsubscribe/1
        , publish/2 ]).

-define(PS_TAG, app_ctrl).

subscribe(EventTag) ->
    gproc_ps:subscribe(l, {?PS_TAG, EventTag}).

unsubscribe(EventTag) ->
    gproc_ps:unsubscribe(l, {?PS_TAG, EventTag}).

publish(EventTag, Info) ->
    gproc_ps:publish(l, {?PS_TAG, EventTag}, Info).
