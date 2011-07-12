-record(child, {
          id :: term(),
          start_func :: {module(), atom(), [term()] | undefined } | undefined,
          restart = permanent :: permanent | transient | temporary,
          shutdown = 5000 :: brutal_kill | timeout() | infinity,
          type :: worker | supervisor,
          modules :: [module()] | dynamic | undefined
         }).

-record(worker, {
          id :: term(),
          start_func :: {module(), atom(), [term()] | undefined } | undefined,
          restart = permanent:: permanent | transient | temporary,
          shutdown = 5000 :: brutal_kill | timeout() | infinity,
          modules :: [module()] | dynamic | undefined
         }).

-record(supervisor, {
          id :: term(),
          registered = false :: term(),
          restart_strategy = one_for_one :: one_for_one | one_for_all | rest_for_one | 
                                   simple_one_for_one,
          max_restarts = {1, 60} :: {pos_integer(), pos_integer()},
          children = [] :: [gen_supervisor:supervisable()],
          %%
          restart = permanent :: permanent | transient | temporary,
          shutdown = infinity :: brutal_kill | timeout() | infinity
         }).
        
-record(one_for_one, {
          id :: term(),
          registered = false :: term(),
          max_restarts = {1,60} :: {pos_integer(), pos_integer()},
          children = [] :: [gen_supervisor:supervisable()],
          %%
          restart = permanent :: permanent | transient | temporary,
          shutdown = infinity :: brutal_kill | timeout() | infinity
         }).

-record(one_for_all, {
          id :: term(),
          registered = false :: term(),
          max_restarts = {1,60} :: {pos_integer(), pos_integer()},
          children = [] :: [gen_supervisor:supervisable()],
          %%
          restart = permanent :: permanent | transient | temporary,
          shutdown = infinity :: brutal_kill | timeout() | infinity
         }).

-record(rest_for_one, {
          id :: term(),
          registered = false :: term(),
          max_restarts = {1,60} :: {pos_integer(), pos_integer()},
          children = [] :: [gen_supervisor:supervisable()],
          %%
          restart = permanent :: permanent | transient | temporary,
          shutdown = infinity :: brutal_kill | timeout() | infinity
         }).

-record(simple_one_for_one, {
          id :: term(),
          registered = false :: term(),
          max_restarts = {1,60} :: {pos_integer(), pos_integer()},
          children = [] :: [gen_supervisor:supervisable()],
          %%
          restart = permanent :: permanent | transient | temporary,
          shutdown = infinity :: brutal_kill | timeout() | infinity
         }).
          
