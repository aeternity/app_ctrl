

# Module app_ctrl #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

API module for app_ctrl.

Copyright (c) 2018-22 Aeternity Anstalt

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).

<a name="description"></a>

## Description ##
This module provides the main user interface to app_ctrl.
<a name="types"></a>

## Data Types ##




### <a name="type-app">app()</a> ###


<pre><code>
app() = atom()
</code></pre>




### <a name="type-app_status">app_status()</a> ###


<pre><code>
app_status() = running | ok_to_start | load_only
</code></pre>




### <a name="type-mode">mode()</a> ###


<pre><code>
mode() = atom()
</code></pre>




### <a name="type-role">role()</a> ###


<pre><code>
role() = atom()
</code></pre>




### <a name="type-status">status()</a> ###


<pre><code>
status() = #{current_mode =&gt; <a href="#type-mode">mode()</a>, current_roles =&gt; [<a href="#type-role">role()</a>], running_locally =&gt; [{<a href="#type-app">app()</a>, [node()]}], running_remotely =&gt; [{<a href="#type-app">app()</a>, [node()]}], allowed_apps =&gt; [<a href="#type-app">app()</a>], role_apps =&gt; [{<a href="#type-app">app()</a>, <a href="#type-app_status">app_status()</a>}]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#app_dependencies-1">app_dependencies/1</a></td><td>Lists the applications on which <code>App</code> depends.</td></tr><tr><td valign="top"><a href="#await_stable_mode-1">await_stable_mode/1</a></td><td>Waits for the system to become stable (no ongoing mode transition)
and returns the active mode, or timeout, including which apps are still
pending.</td></tr><tr><td valign="top"><a href="#check_for_new_applications-0">check_for_new_applications/0</a></td><td>Checks if applications have been added (or removed) and takes
appropriate action.</td></tr><tr><td valign="top"><a href="#dependencies_of-1">dependencies_of/1</a></td><td>List all applications that depend on <code>App</code>
(including <code>start_before</code> dependencies.</td></tr><tr><td valign="top"><a href="#get_mode-0">get_mode/0</a></td><td>Get the current mode.</td></tr><tr><td valign="top"><a href="#is_mode_stable-0">is_mode_stable/0</a></td><td>Checks whether the system is in a stable mode (<code>true</code>), or
in transition between modes (<code>false</code>).</td></tr><tr><td valign="top"><a href="#set_and_await_mode-2">set_and_await_mode/2</a></td><td>Like <a href="#set_mode-1"><code>set_mode/1</code></a>, but waits until the transition to the
new mode has been completed.</td></tr><tr><td valign="top"><a href="#set_mode-1">set_mode/1</a></td><td>Sets the current mode to <code>Mode</code>.</td></tr><tr><td valign="top"><a href="#status-0">status/0</a></td><td>Returns information about the current state of the system.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="app_dependencies-1"></a>

### app_dependencies/1 ###

<pre><code>
app_dependencies(App) -&gt; [<a href="#type-app">app()</a>]
</code></pre>

<ul class="definitions"><li><code>App = <a href="#type-app">app()</a></code></li></ul>

Lists the applications on which `App` depends.

<a name="await_stable_mode-1"></a>

### await_stable_mode/1 ###

<pre><code>
await_stable_mode(Timeout) -&gt; {ok, <a href="#type-mode">mode()</a>} | {timeout, [<a href="#type-app">app()</a>]}
</code></pre>

<ul class="definitions"><li><code>Timeout = non_neg_integer() | infinity</code></li></ul>

Waits for the system to become stable (no ongoing mode transition)
and returns the active mode, or timeout, including which apps are still
pending.

<a name="check_for_new_applications-0"></a>

### check_for_new_applications/0 ###

<pre><code>
check_for_new_applications() -&gt; ok
</code></pre>
<br />

Checks if applications have been added (or removed) and takes
appropriate action.

<a name="dependencies_of-1"></a>

### dependencies_of/1 ###

<pre><code>
dependencies_of(App) -&gt; [<a href="#type-app">app()</a>]
</code></pre>

<ul class="definitions"><li><code>App = <a href="#type-app">app()</a></code></li></ul>

List all applications that depend on `App`
(including `start_before` dependencies.

<a name="get_mode-0"></a>

### get_mode/0 ###

<pre><code>
get_mode() -&gt; <a href="#type-mode">mode()</a>
</code></pre>
<br />

Get the current mode.

<a name="is_mode_stable-0"></a>

### is_mode_stable/0 ###

<pre><code>
is_mode_stable() -&gt; boolean()
</code></pre>
<br />

Checks whether the system is in a stable mode (`true`), or
in transition between modes (`false`).

<a name="set_and_await_mode-2"></a>

### set_and_await_mode/2 ###

<pre><code>
set_and_await_mode(Mode, Timeout) -&gt; {ok, Mode} | {timeout, [<a href="#type-app">app()</a>]}
</code></pre>

<ul class="definitions"><li><code>Mode = <a href="#type-mode">mode()</a></code></li><li><code>Timeout = non_neg_integer() | infinity</code></li></ul>

Like [`set_mode/1`](#set_mode-1), but waits until the transition to the
new mode has been completed.

<a name="set_mode-1"></a>

### set_mode/1 ###

<pre><code>
set_mode(Mode::<a href="#type-mode">mode()</a>) -&gt; ok
</code></pre>
<br />

Sets the current mode to `Mode`.
Will raise an exception if `Mode` is not a known mode.

<a name="status-0"></a>

### status/0 ###

<pre><code>
status() -&gt; <a href="#type-status">status()</a>
</code></pre>
<br />

Returns information about the current state of the system.

