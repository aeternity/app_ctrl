%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(app_ctrl_config).

-export([
          roles/0
        , modes/0
        , default_mode/0
        , applications/0
        , init_apps/0
        ]).

-include_lib("kernel/include/logger.hrl").

roles() -> get_env_set(roles).

modes() -> get_env_set(modes).

default_mode() ->
    case get_original_env(default_mode) of
        [] ->
            undefined;
        Val ->
            Val
    end.

%% Format: [{App, 
applications() -> get_env_dict(applications).

init_apps() -> get_env_set(init_apps).

get_env_set(Key) ->
    Orig = ordsets:from_list(get_original_env(Key)),
    ?LOG_DEBUG("Orig (Key = ~p) = ~p", [Key, Orig]),
    get_apps_env_set(Key, Orig).

get_env_dict(Key) ->
    Orig = orddict:from_list(get_original_env(Key)),
    ?LOG_DEBUG("Orig (Key = ~p) = ~p", [Key, Orig]),
    get_apps_env_dict(Key, Orig).

get_apps_env_set(Key, Acc0) ->
    fold_envs(
      fun(join, App, Acc1) ->
              ordsets:add_element(App, Acc1);
         (leave, App, Acc1) ->
              ordsets:del_element(App, Acc1);
         ({add, Vals}, _, Acc1) ->
              ordsets:union(ordsets:from_list(Vals), Acc1);
         ({del, Vals}, _, Acc1) ->
              ordsets:subtract(Acc1, ordsets:from_list(Vals))
      end, Acc0, Key).

get_apps_env_dict(Key, Acc0) ->
    fold_envs(
      fun({add, NewVals}, _App, Acc1) ->
              orddict:merge(
                fun(K, _V1, V2) ->
                        ?LOG_DEBUG("Redefining ~p: ~p", [K, V2]),
                        V2
                end, Acc1, orddict:from_list(NewVals));
         ({del, Keys}, _App, Acc1) ->
              orddict:filter(fun(K,_) ->
                                     not lists:member(K, Keys)
                             end, Acc1)
      end, Acc0, Key).

fold_envs(Fun, Acc0, Key) ->
    lists:foldr(
      fun({App, Env}, Acc) ->
              lists:foldr(
                fun(X, Acc1) ->
                        Fun(X, App, Acc1)
                end, Acc, find_modify_actions(Key, Env))
      end, Acc0, setup:find_env_vars('$app_ctrl')).

find_modify_actions(Key, Env) ->
    proplists:get_value(
      Key, proplists:get_value(modify, Env, []), []).

get_original_env(Key) ->
    case get_original_env_(Key) of
        [] ->
            [];
        [{_, Val}] ->
            Val;
        [_|_] = Duplicates ->
            ?LOG_ERROR("Duplicate app_ctrl env entries: ~p", [Duplicates]),
            error(duplicate_app_ctrl_entries)
    end.

get_original_env_(Key) ->
    case application:get_env(app_ctrl, Key) of
        {ok, Val} ->
            [{app_ctrl, Val} | get_app_ctrl_envs(Key)];
        undefined ->
            get_app_ctrl_envs(Key)
    end.

get_app_ctrl_envs(Key) ->
    lists:foldr(
      fun({A, Env}, Acc) ->
              case lists:keyfind(Key, 1, Env) of
                  false ->
                      Acc;
                  {_, V} ->
                      [{A, V}|Acc]
              end
      end, [], setup:find_env_vars('$app_ctrl')).
