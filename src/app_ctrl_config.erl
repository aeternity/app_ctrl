%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(app_ctrl_config).

-export([
          roles/0
        , modes/0
        , current_mode/0
        , set_current_mode/1
        , default_mode/0
        , applications/0
        , protected_mode_apps/0
        ]).

-include_lib("kernel/include/logger.hrl").

-define(CORE_PROTECTED_MODE_APPS, [setup, gproc, app_ctrl]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

roles() -> get_env_dict(roles, set).

modes() -> get_env_dict(modes, set).

current_mode() ->
    case application:get_env(app_ctrl, mode) of
        undefined ->
            default_mode();
        {ok, Val} ->
            Val
    end.

set_current_mode(Mode) ->
    ?LOG_DEBUG("set_current_mode(~p)", [Mode]),
    application:set_env(app_ctrl, mode, Mode).

default_mode() ->
    case get_original_env(default_mode) of
        [] ->
            undefined;
        Val ->
            Val
    end.

%% Format: [{App, 
applications() -> get_env_dict(applications, dict).

protected_mode_apps() -> get_env_set(protected_mode_apps).

get_env_set(Key) ->
    Orig = ordsets:from_list(get_original_env(Key)),
    ?LOG_DEBUG("Orig (Key = ~p) = ~p", [Key, Orig]),
    AppEnvs = setup:find_env_vars('$app_ctrl'),
    get_apps_env_set(Key, Orig, AppEnvs).

get_env_dict(Key, SubsetType) ->
    Orig = orddict:from_list(get_original_env(Key)),
    ?LOG_DEBUG("Orig (Key = ~p) = ~p", [Key, Orig]),
    get_apps_env_dict(Key, Orig, SubsetType).

get_apps_env_set(Key, Acc0, AppEnvs) ->
    fold_envs(
      fun(join, App, Acc1) ->
              ordsets:add_element(App, Acc1);
         (leave, App, Acc1) ->
              ordsets:del_element(App, Acc1);
         ({add, Vals}, _, Acc1) ->
              ordsets:union(ordsets:from_list(Vals), Acc1);
         ({del, Vals}, _, Acc1) ->
              ordsets:subtract(Acc1, ordsets:from_list(Vals))
      end, Acc0, Key, AppEnvs).

get_apps_env_dict(Key, Acc0, SubsetType) ->
    get_apps_env_dict(Key, Acc0, SubsetType, setup:find_env_vars('$app_ctrl')).

get_apps_env_dict(Key, Acc0, SubsetType, AppEnvs) ->
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
                             end, Acc1);
         ({SubKey, Actions}, App, Acc1) when is_atom(SubKey)
                                           , is_list(Actions) ->
              orddict_apply_actions(SubKey, Actions, App, SubsetType, Acc1);
         (Op, _, _) ->
              error({illegal_modify_action, Op})
      end, Acc0, Key, AppEnvs).

orddict_apply_actions(Key, Actions, App, SubsetType, Dict) ->
    Val = case orddict:find(Key, Dict) of
              {ok, Found} -> ensure_set(Found, SubsetType);
              error       -> empty(SubsetType)
          end,
    NewVal =
        lists:foldl(
          fun(join, V) when SubsetType == set ->
                  assert_member(false, App, V, set),
                  ordsets:add_element(App, V);
             (leave, V) when SubsetType == set ->
                  assert_member(true, App, V, set),
                  ordsets:del_element(App, V);
             ({add, Vals}, V) ->
                  [assert_member(false, X, V, SubsetType) || X <- Vals],
                  add_elements(V, Vals, SubsetType);
             ({del, Vals}, V) ->
                  [assert_member(true, X, V, SubsetType) || X <- Vals],
                  del_elements(V, Vals, SubsetType);
             (Op, _) ->
                  error({illegal_modify_action, Op})
          end, Val, Actions),
    orddict:store(Key, NewVal, Dict).

assert_member(Bool, Key, Set, SetType) ->
    IsMember = case SetType of
                   set  -> lists:member(Key, Set);
                   dict -> lists:keymember(Key, 1, Set)
               end,
    case {Bool, IsMember} of
        {B, B} ->
            ok;
        {false, _} ->
            error({already_member, Key});
        {true, _} ->
            error({not_member, Key})
    end.

ensure_set(L, set) ->
    ordsets:from_list(L);
ensure_set(L, dict) ->
    orddict:from_list(L).

empty(set) ->
    ordsets:new();
empty(dict) ->
    orddict:new().

add_elements(Set, Elems, set) ->
    ordsets:union(Set, ordsets:from_list(Elems));
add_elements(Set, Elems, dict) ->
    orddict:merge(fun(_Key, As1, As2) ->
                          ordsets:union(ordsets:from_list(As1),
                                        ordsets:from_list(As2))
                  end, Set, orddict:from_list(Elems)).

del_elements(Set, Elems, set) ->
    lists:foldl(fun ordsets:del_element/2, Set, Elems);
del_elements(Set, Elems, dict) ->
    lists:foldl(fun orddict:erase/2, Set, Elems).

%% fold_envs(Fun, Acc0, Key) ->
%%     fold_envs(Fun, Acc0, Key, setup:find_env_vars('$app_ctrl')).

fold_envs(Fun, Acc0, Key, EnvVars) ->
    lists:foldr(
      fun({App, Env}, Acc) ->
              lists:foldr(
                fun(X, Acc1) ->
                        Fun(X, App, Acc1)
                end, Acc, find_modify_actions(Key, Env))
      end, Acc0, EnvVars).

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

get_original_env_(protected_mode_apps) ->
    [{app_ctrl, ?CORE_PROTECTED_MODE_APPS}];
get_original_env_(Key) ->
    case application:get_env(app_ctrl, Key) of
        {ok, Val} ->
            [{app_ctrl, Val} | get_app_ctrl_envs(Key)];
        undefined ->
            remove_modifiers(
              get_app_ctrl_envs(Key))
    end.

remove_modifiers(Envs) ->
    lists:foldr(
      fun({App, E}, Acc) ->
              case remove_modifiers_(E) of
                  [] ->
                      Acc;
                  [_|_] = Rest ->
                      [{App, Rest}|Acc]
              end
      end, [], Envs).

remove_modifiers_(Env) ->
    lists:foldr(
      fun({modify,_}, Acc) ->
              Acc;
         ({Key, Sub}, Acc) ->
              case lists:filter(fun({modify,_}) -> false;
                                   (_) -> true
                                end, Sub) of
                  [] ->
                      Acc;
                  Rest ->
                      [{Key, Rest}|Acc]
              end
      end, [], Env).

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


-ifdef(TEST).

join_role_test() ->
    [{role_a, [a,b,c]},
     {role_b, [d,e]}] =
        get_apps_env_dict(roles, [{role_a, [a,b]},
                                  {role_b, [d,e]}], set, [{c, [{modify,
                                                                [{roles,
                                                                  [{role_a, [join]}]
                                                                 }]
                                                               }]
                                                          }]),
    ok.

role_add_app_test() ->
    [{role_a, [a,b,c,d]},
     {role_b, [d,e]}] =
        get_apps_env_dict(roles, [{role_a, [a,d]},
                                  {role_b, [d,e]}], set, [{q, [{modify,
                                                                [{roles,
                                                                  [{role_a, [{add, [c,b]}]
                                                                   }]
                                                                 }]
                                                               }]
                                                          }]),
    ok.

role_join_add_test() ->
    [{role_a, [a,b,c,d]},
     {role_b, [d,e]}] =
        get_apps_env_dict(roles, [{role_a, [a,d]},
                                  {role_b, [d,e]}], set, [{c, [{modify,
                                                                [{roles,
                                                                  [{role_a, [join,
                                                                             {add, [b]}]
                                                                   }]
                                                                 }]
                                                               }]
                                                          }]),
    ok.

role_leave_test() ->
    [{role_a, [a,b,d]},
     {role_b, [d,e]}] =
        get_apps_env_dict(roles, [{role_a, [a,b,c,d]},
                                  {role_b, [d,e]}], set, [{c, [{modify,
                                                                [{roles,
                                                                  [{role_a, [leave]}]
                                                                 }]
                                                               }]
                                                          }]),
    ok.

role_join_leave_test() ->
    [{role_a, [a,b,d]},
     {role_b, [c,d,e]}] =
        get_apps_env_dict(roles, [{role_a, [a,b,c,d]},
                                  {role_b, [d,e]}], set, [{c, [{modify,
                                                                [{roles,
                                                                  [{role_a, [leave]},
                                                                   {role_b, [join]}]
                                                                 }]
                                                               }]
                                                          }]),
    ok.

role_join_empty_test() ->
    [{role_a, [a]}] =
        get_apps_env_dict(roles, [], set, [{a, [{modify,
                                                 [{roles,
                                                   [{role_a, [join]}]
                                                   }]
                                                }]
                                           }]),
    ok.

role_leave_error_test() ->
    try get_apps_env_dict(roles, [], set, [{a, [{modify,
                                                 [{roles,
                                                   [{role_a, [leave]}]
                                                  }]
                                                }]
                                           }]) of
        Any ->
            error({unexpected, Any})
    catch
        error:{not_member, a} ->
            ok
    end.

role_join_error_test() ->
    try get_apps_env_dict(roles, [{role_a, [a]}], set, [{a, [{modify,
                                                              [{roles,
                                                                [{role_a, [join]}]
                                                               }]
                                                             }]
                                                        }]) of
        Any ->
            error({unexpected, Any})
    catch
        error:{already_member, a} ->
            ok
    end.

app_add_test() ->
    [{a, [{start_before,[b]}]}] =
        get_apps_env_dict(applications, [], dict, [{q, [{modify,
                                                         [{applications,
                                                           [{add, [{a, [{start_before,[b]}]}]}]
                                                          }]
                                                        }]
                                                   }]),
    ok.

app_add_append_test() ->
    [{a, [{start_before, [b,c]}]}] =
        get_apps_env_dict(applications, [{a,
                                          [{start_before, [b]}]
                                         }], dict,
                          [{q, [{modify,
                                 [{applications,
                                   [{a,
                                     [{add, [{start_before,[c]}]}]
                                    }]
                                  }]
                                }]
                           }]),
    ok.

app_del_test() ->
    [{a,[]}] =
        get_apps_env_dict(applications, [{a, [{start_before, [b]}]}], dict,
                          [{q, [{modify,
                                 [{applications,
                                   [{a, [{del, [start_before]}]}]
                                  }]
                                }]
                           }]),
    ok.

app_del_app_test() ->
    [] =
        get_apps_env_dict(applications, [{a, [{start_before, [b]}]}], dict,
                          [{q, [{modify,
                                 [{applications,
                                   [{del, [a]}]
                                  }]
                                }]
                           }]),
    ok.

modes_add_test() ->
    [{development, [mining_tools]}] =
        get_apps_env_dict(modes, [], set, [{a, [{modify,
                                                 [{modes,
                                                   [{development,
                                                     [{add, [mining_tools]}]
                                                    }]
                                                  }]
                                                }]
                                           }]),
    ok.

modes_add_role_test() ->
    [{development, [mining_tools]}] =
        get_apps_env_dict(modes, [], set, [{a, [{modify,
                                                 [{modes,
                                                   [{add,
                                                     [{development, [mining_tools]}]
                                                    }]
                                                  }]
                                                }]
                                           }]),
    ok.

protected_mode_apps_test() ->
    [a,b,c,d,e] =
        get_apps_env_set(protected_mode_apps, [a,b,e], [{q, [{modify,
                                                              [{protected_mode_apps,
                                                                [{add, [c,d]}]
                                                               }]
                                                            }]
                                                       }]),
    ok.

-endif.
