%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(app_ctrl_config).

-export([ 
          adding_handler/1
        , changing_config/3
        , filter_config/1
        , log/2
        , removing_handler/1
        ]).

%%% ======================================================================
%%% Logger callbacks

%%% Handler being added
-spec adding_handler(Config) -> {ok,Config} | {error,Reason} when
      Config :: logger:handler_config(),
      Reason :: term().
adding_handler(Config) ->
    io:fwrite("adding_handler(~p)~n", [Config]),
    io:fwrite("Loaded = ~p~n", [application:loaded_applications()]),
    Pid = spawn_dac(),
    {ok, Config#{ulf => rules, dac => Pid}}.

%%% Updating handler config
-spec changing_config(SetOrUpdate, OldConfig, NewConfig) ->
                              {ok,Config} | {error,Reason} when
      SetOrUpdate :: set | update,
      OldConfig :: logger:handler_config(),
      NewConfig :: logger:handler_config(),
      Config :: logger:handler_config(),
      Reason :: term().
changing_config(_SetOrUpdate, _OldConfig, NewConfig) ->
    {ok, NewConfig}.

%%% Remove internal fields from configuration
-spec filter_config(Config) -> Config when
      Config :: logger:handler_config().
filter_config(Config) ->
    Config.

%%% Log a string or report
-spec log(LogEvent, Config) -> ok when
      LogEvent :: logger:log_event(),
      Config :: logger:handler_config().
log(_Event, _Config) ->
    ok.

%%% Handler being removed
-spec removing_handler(Config) -> ok when
      Config :: logger:handler_config().
removing_handler(_Config) ->
    ok.

%%% End logger callbacks
%%% ======================================================================

spawn_dac() ->
    {ok, Pid} = app_ctrl_server:start(),
    Pid.
%%     Me = self(),
%%     {Pid, MRef} = spawn_monitor(
%%                     fun() ->
%%                             init_dac(Me)
%%                     end),
%%     receive
%%         {Pid, ok} ->
%%             demonitor(MRef),
%%             Pid;
%%         {'DOWN', MRef, _, _, Reason} ->
%%             error(Reason)
%%     end.

%% init_dac(Parent) ->
%%     io:fwrite("DAC here!!! AC = ~p~n", [whereis(application_controller)]),
%%     LoadRes = application:load(app_ctrl),
%%     io:fwrite("LoadRes = ~p~n", [LoadRes]),
%%     CtrlApps = application:get_env(app_ctrl, apps, []),
%%     io:fwrite("CtrlApps = ~p~n", [CtrlApps]),
%%     CtrlRes = [{A, application_controller:control_application(A)} || A <- CtrlApps],
%%     io:fwrite("CtrlRes = ~p~n", [CtrlRes]),
%%     register(dist_ac, self()),
%%     Parent ! {self(), ok},
%%     dac_loop().

%% dac_loop() ->
%%     receive
%%         Msg ->
%%             io:fwrite("dac: ~p~n", [Msg])
%%     end,
%%     dac_loop().
