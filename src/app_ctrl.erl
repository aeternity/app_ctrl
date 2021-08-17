-module(app_ctrl).

-export([ status/0
        , dependencies_of/1
        , app_dependencies/1
        , get_mode/0
        , set_mode/1
        , set_and_await_mode/2
        , is_mode_stable/0
        , await_stable_mode/1 ]).

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

set_and_await_mode(Mode, Timeout) ->
    set_mode(Mode),
    await_stable_mode(Timeout).

is_mode_stable() ->
    app_ctrl_server:is_mode_stable().

await_stable_mode(Timeout) when Timeout == infinity
                              ; is_integer(Timeout), Timeout > 0 ->
    app_ctrl_server:await_stable_mode(Timeout).
