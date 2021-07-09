-module(app_ctrl).

-export([dependencies_of/1,
         app_dependencies/1]).

app_dependencies(App) ->
    G = app_ctrl_server:graph(),
    app_ctrl_deps:get_app_dependencies(G, App).

dependencies_of(App) ->
    G = app_ctrl_server:graph(),
    app_ctrl_deps:get_dependencies_of_app(G, App).
