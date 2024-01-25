# app_ctrl

**Author:** Ulf Wiger <ulf@wiger.net>

This application implements an OTP distributed application controller.

Currently, only local application control is supported, but one particular
feature is that it allows applications to be started *before* another
application.

**[API documentation](API_DOCS.md)**

## Background

The standard application controller respects dependencies specified in
an application's `.app` file, where the `applications` attribute lists
other applications that must be running before the given application can
be started.

In some cases, one may want to ensure that a certain application is started
before a third-party application, whose `.app` file is already set.

For example:

* An application running before mnesia, which checks conditions and
  e.g. creates a database schema or dynamically calculates
  `extra_db_nodes` (this must be done before mnesia starts).
* Applications needed by `exometer_core` probes, which could then be
  declaratively defined and started automatically by `exometer_core`.

Currently, only local application control is supported.

## Implementation

The `app_ctrl` application controller server bootstraps itself in the `kernel`
application start process (via a dummy logger handler - see below), and then
spawns individual controller processes for the applications that need to be
controlled. Specifically, this is all applications listed in the `app_ctrl`
environment variables `applications` and `roles`. All other applications
will be considered background apps, and are started as usual by the OTP
`application_controller`.

For a description of the protocol used between the `application_controller`
and the controller processes, see comments in `application_controller.erl`.

## Event notifications

The `app_ctrl` application provides a pub-sub interface (based on `gproc_ps`)
for keeping track of application start events. To subscribe, call
`app_ctrl_events:subscribe(EventCategory)`. Notifications will be delivered
as messages of the form `{gproc_ps_event, {app_ctrl, Event}, Info}`, where
the events can be:

* `Event :: app_running | app_stopped`, `Info :: {app_name(), node()}`
* `Event :: new_mode`, `Info :: mode_name()`

Note that `new_mode` signals the intention to switch to a new mode, or
possibly that the definition of a mode currently being implemented has been
refreshed. In order to await the completion of a mode shift, use
`app_ctrl:await_stable_mode()`.

## Configuring `app_ctrl`

One idea is to automate parts of the configuration, e.g. through a rebar3
plugin, but currently, all configuration needs to be done manually.

* Set the `app_ctrl` application environment variable `applications`.

```erlang
{app_ctrl, [
   {applications, [
      {gproc, [{start_before, [exometer_core]}]}
    ]},
   {roles, [
      {basic, []},
      {active, [aecore, aechannel, aehttp, aestratum]}
    ]},
   {modes, [
      {normal, [basic, active]},
      {maintenance, [basic]}
   ]}
 ]}
```
* In order for the application control to work best, a dummy `logger`
  handler should be installed like so:

```erlang
{kernel, [
   {logger, [
      {handler, app_ctrl, app_ctrl_bootstrap, #{}}
    ]}
 ]}
```
Also, the `app_ctrl` application must be installed and started.

Note that any log level or filter settings for the above `app_ctrl`
handler will be overwritten by the handler itself.
It is not intended for reacting to log messages.

Inspecting the logger handler via `logger:i()` will reveal something like this:

```
    Id: app_ctrl
        Module: app_ctrl_config
        Level:  none
        Formatter:
            Module: logger_formatter
            Config:
                []
        Filter Default: stop
        Filters:
            (none)
        Custom Config:
            app_ctrl_server: <0.1959.0>
```

If the handler hasn't been configured to install at startup, it will be
added by the `app_ctrl` application as soon as it starts. So if your
system relies on a custom startup procedure, calling
`application:ensure_all_started(app_ctrl)` as one of the first steps
should also work.

Note, however, that there will then be a gap between the start of the
mandatory `kernel` and `stdlib` apps, and the start of `app_ctrl`,
and this might create a window where applications are started before
`app_ctrl` is able to take control of them.

### Configuration details

The `app_ctrl` configuration can either be defined in the `app_ctrl` app
environment (usually via `sys.config`), or in each application's local app
environment. In the latter case, settings are given under the environment key
`$app_ctrl`, either defining the `app_ctrl` settings in one place, or
incrementally modifying settings under a `modify` rubric as described below.

#### `applications`

The `applications` list is mainly intended to allow certain applications
to be started _before_ other applications. This is not possible to specify
in the OTP application controller, but can be useful e.g. in order to ensure
that a preparatory application gets to run before some third-party app whose
`applications` dependencies can't easily be changed.

**Example:**
```erlang
{applications, [
    {setup, [{start_before, [mnesia]}]}
 ]}
```
When defining the setting inside another application, the configuration is:

```erlang
{'$app_ctrl', [
  {applications, [
    {setup, [{start_before, [mnesia]}]}
   ]}
 ]}
```
When incrementally modifying the setting from a local environment, use:

```erlang
{'$app_ctrl', [
  {modify, [
    {applications, [
      {Action, Values}
     ]}
 ]}
```

Where `Action` is one of:

* `add` - `Values:: [{app_name(), dependency()}]`
  Add a list application dependencies, possibly replacing entries
  which are already in the list.
* `del` - `Values :: [app_name()]`
  Remove applications if they are present in the list

When using the local application environment to inform `app_ctrl`

#### `roles`

The `{roles, [app_name()]}` list allows for grouping of applications into more
manageable categories. This is to make a distributed layout (not yet supported)
more readable.

**Example:**
```erlang
{roles, [
  {basic, [setup, gproc]}
 ]}
```

When incrementally modifying in the local app environment, the following actions
are supported:

* `join` - the current application is added to the set
* `leave` - the current application is removed from the set
* `{add, [app_name()]}` add applications to the set
* `{del, [app_name()]}` remove applications from the set

**Example:**
```erlang
{'$app_ctrl', [
  {modify, [
    {roles, [
      {basic, [join]}  % The current app joins the `basic` role
   ]}
 ]}
```

#### `modes`

The `{modes, [{mode_name(), [role()]}]}` list allows for specification of
different processor modes, where different sets of `roles` are applied.

When incrementally modifying in the local app environment, the following
actions are supported:

**Example:**
```erlang
{'$app_ctrl', [
  {modify, [
    {modes, [
      {development, [
        {add, [mining_tools]}
       ]}
   ]}
 ]}
```

## Build

    $ rebar3 compile

## Debug

`app_ctrl` uses `logger` logging, and checks an application environment variable
for detailed module-level filtering of logging output.

By setting the `app_ctrl` variable `log_levels` to `[{Level, [Mod]}]`, specific
modules can be made to output more or less debugging info.
