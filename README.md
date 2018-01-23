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

## Configuring `app_ctrl`

The idea is to automate parts of the configuration, e.g. through a rebar3
plugin, but currently, all configuration needs to be done manually.

* Set the `app_ctrl` application environment variable `applications`.
  The option of interest for now is `start_before`:

```erlang
{app_ctrl, [{applications,
             [
              {gproc, [{start_before, [exometer_core]}]}
             ]}
            ]}
```

* In order for the application control to work, permissions must also
  Be set to `false` for the affected applications. The `app_ctrl`
  application will set all permissions to `true` as soon as it is set up
  and has acquired control of the relevant applications.

```erlang
{kernel, [{permissions,
           [
            {gproc, false},
            {exometer_core, false}
           ]}
         ]}
```

## Build

    $ rebar3 compile
