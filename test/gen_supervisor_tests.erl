-module(gen_supervisor_tests).
-include_lib("eunit/include/eunit.hrl").

t_starts_supervisor() ->
    ?assertMatch([{some_sup, _, _, _},{first, _, _, _}], supervisor:which_children(test_sup)),
    ?assertMatch(Pid when is_pid(Pid), whereis(some_sup)),
    ?assertEqual(undefined, whereis(first)).


t_nested_supervisor() ->
    [{some_sup, Pid, _, _},{first, _, _, _}] = supervisor:which_children(test_sup),
    ?assertMatch([{some, _, _, _}], supervisor:which_children(Pid)).

gen_supervisor_test_() ->
     {setup,
      fun () ->
              application:start(gen_supervisor),
              {ok, SupPid} = test_sup:start_link(),
              SupPid
      end,
      fun (SupPid) ->
              application:stop(gen_supervisor)
      end,
     [
      {"starts supervisor", ?_test(t_starts_supervisor())},
      {"nested supervisor is handled properly", ?_test(t_nested_supervisor())}

     ]}.
