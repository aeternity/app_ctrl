-module(app_ctrl).

-export([ status/0
        , dependencies_of/1
        , app_dependencies/1
        , get_mode/0
        , set_mode/1 ]).

status() ->
    app_ctrl_server:status().

app_dependencies(App) ->
    G = app_ctrl_server:graph(),
    app_ctrl_deps:get_app_dependencies(G, App).

dependencies_of(App) ->
    G = app_ctrl_server:graph(),
    app_ctrl_deps:get_dependencies_of_app(G, App).

get_mode() ->
    app_ctrl_server:get_mode().

set_mode(Mode) ->
    case lists:keymember(Mode, 1, app_ctrl_config:modes()) of
        true ->
            app_ctrl_server:set_mode(Mode);
        false ->
            error(unknown_mode)
    end.
