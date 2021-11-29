%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(app_ctrl_bootstrap).

-export([ 
          adding_handler/1
        , changing_config/3
        , filter_config/1
        , log/2
        , removing_handler/1
        ]).

-export([ update_server_pid/1 ]).

-include_lib("kernel/include/logger.hrl").

%%% ======================================================================
%%% State updates

update_server_pid(Pid) when is_pid(Pid) ->
    {ok, #{config := HConf}} = logger:get_handler_config(app_ctrl),
    logger:update_handler_config(
      app_ctrl, config, HConf#{app_ctrl_server => Pid}).

%%% ======================================================================
%%% Logger callbacks

%%% Handler being added
-spec adding_handler(Config) -> {ok,map()} | {error,Reason} when
      Config :: logger:handler_config(),
      Reason :: term().
adding_handler(Config) ->
    erlang:display(#{msg => "adding bootstrap handler"}),
    ?LOG_DEBUG("adding_handler(~p)", [Config]),
    Pid = spawn_app_ctrl(),
    Config1 = Config#{ filters => []
                     , level => none
                     , filter_default => stop
                     , config => #{app_ctrl_server => Pid} },
    {ok, Config1}.

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

spawn_app_ctrl() ->
    {ok, Pid} = app_ctrl_server:start(),
    ?LOG_DEBUG("Dac pid: ~p", [Pid]),
    Pid.
