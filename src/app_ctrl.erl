-module(app_ctrl).

-export([dependencies_of/1]).



dependencies_of(App) ->
    G = app_ctrl_server:graph(),
    app_ctrl_deps:get_dependencies_of_app(G, App).
