

# Module app_ctrl_events #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module provides pub/sub services for app_ctrl.

Copyright (c) 2018-22 Aeternity Anstalt

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).

<a name="types"></a>

## Data Types ##




### <a name="type-event_tag">event_tag()</a> ###


<pre><code>
event_tag() = app_running | app_stopped | new_mode
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td>Subscribe to events with the given event tag.</td></tr><tr><td valign="top"><a href="#unsubscribe-1">unsubscribe/1</a></td><td>Unsubscribe from events with the given event tag.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="subscribe-1"></a>

### subscribe/1 ###

<pre><code>
subscribe(EventTag) -&gt; true
</code></pre>

<ul class="definitions"><li><code>EventTag = <a href="#type-event_tag">event_tag()</a></code></li></ul>

Subscribe to events with the given event tag.
See [`gproc_ps:subscribe/2`](https://github.com/uwiger/gproc/blob/master/doc/gproc_ps.md#subscribe-2)

<a name="unsubscribe-1"></a>

### unsubscribe/1 ###

<pre><code>
unsubscribe(EventTag) -&gt; true
</code></pre>

<ul class="definitions"><li><code>EventTag = <a href="#type-event_tag">event_tag()</a></code></li></ul>

Unsubscribe from events with the given event tag.
See [`gproc_ps:unsubscribe/2`](https://github.com/uwiger/gproc/blob/master/doc/gproc_ps.md#unsubscribe-2)

