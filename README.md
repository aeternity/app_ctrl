# app_ctrl

**Author:** Ulf Wiger <ulf@wiger.net>

This application implements an OTP distributed application controller.

Currently, only local application control is supported, but one particular
feature is that it allows applications to be started *before* another
application.

## Background

The standard application controller respects dependencies specified in
an application's `.app` file, where the `applications` attribute lists
other applications that must be running before the given application can
be started.

In some cases, one may want to ensure that a certain application is started
before a third-party application, whose `.app` file is already set.

For example:

* An application running before mnesia, which checks conditions and
  e.g. creates a database schema (this must be done before mnesia starts)
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

## Configuring `app_ctrl`

The idea is to automate parts of the configuration, e.g. through a rebar3
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
* In order for the application control to work, a dummy `logger` handler
  must be installed like so:

```erlang
{kernel, [
   {logger, [
      {handler, app_ctrl, app_ctrl_config, #{}}
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

### Configuration details

#### `applications`

The `applications` list is mainly intended to allow certain applications
to be started _before_ other applications. This is not possible to specify
in the OTP application controller, but can be useful e.g. in order to ensure 
that a preparatory application gets to run before some third-party app whose
`applications` dependencies can't easily be changed.

#### `roles`

The `{roles, [app_name()]}` list allows for grouping of applications into more
manageable categories. This is to make a distributed layout (not yet supported)
more readable.

#### `modes`

The `{modes, [{mode_name(), [role()]}]}` list allows for specification of
different processor modes, where different sets of `roles` are applied.

## Build

    $ rebar3 compile
