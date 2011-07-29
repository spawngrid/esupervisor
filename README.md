esupervisor
===========

`esupervisor` is an extension of Erlang's `supervisor` API that makes it more convenient.

Features
--------

* record-based syntax 

It allows you to avoid problems with the need to follow the exact order of elements in 
`supervisor` tuples so that you don't have to use (often) inconsistent `CHILD` macros and yet
describe your supervision items at any level of detail you want

* single callback to describe the entire supervision tree

Instead of scattering your supervision tree across multiple `sup` modules you can have a clear
view of your entire supervision tree in a single module because `esupervisor` allows you to
specify supervisors as children.


Example
-------

Define your supervisor:

``` erlang
-module(my_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, []).

```

Define your supervision tree:

``` erlang
init([]) ->
    #one_for_one{
      children = [
                  #one_for_one{
                     id = first 
                    },
                  #simple_one_for_one{
                                id = some_sup,
                                registered = some_sup,
                                children = [
                                            #worker{
                                               id = some,
                                               restart = permanent,
                                               start_func = {some_worker, start_link, []}
                                              }
                                           ]
                              }
                 ]
      
     }.
     
```

The above example will define a `one_for_one` supervisor supervising two other supervisors, `first`
and `some_sup` (registered as `some_sup`), and `some_sup` sofo supervisor supervises a worker process
`some` that will be restarted permanently.