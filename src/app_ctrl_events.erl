%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-

%%% @copyright 2018-22 Aeternity Anstalt
%%% @author Ulf Wiger <ulf@wiger.net>
%%% @doc This module provides pub/sub services for app_ctrl.

-module(app_ctrl_events).

-export([ subscribe/1
        , unsubscribe/1
        , publish/2 ]).

-type event_tag() :: app_running | app_stopped | new_mode.

-export_type([event_tag/0]).

-define(PS_TAG, app_ctrl).

-spec subscribe(EventTag) -> true
              when EventTag :: event_tag().
%% @doc Subscribe to events with the given event tag.
%% See <a href="https://github.com/uwiger/gproc/blob/master/doc/gproc_ps.md#subscribe-2"><tt>gproc_ps:subscribe/2</tt></a>
%% @end
subscribe(EventTag) ->
    gproc_ps:subscribe(l, {?PS_TAG, EventTag}).

-spec unsubscribe(EventTag) -> true
              when EventTag :: event_tag().
%% @doc Unsubscribe from events with the given event tag.
%% See <a href="https://github.com/uwiger/gproc/blob/master/doc/gproc_ps.md#unsubscribe-2"><tt>gproc_ps:unsubscribe/2</tt></a>
%% @end
unsubscribe(EventTag) ->
    gproc_ps:unsubscribe(l, {?PS_TAG, EventTag}).

%% @hidden
publish(EventTag, Info) ->
    gproc_ps:publish(l, {?PS_TAG, EventTag}, Info).
