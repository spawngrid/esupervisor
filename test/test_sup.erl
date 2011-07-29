-module(test_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

%% API
-export([start_link/0]).

-export([some_start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    #one_for_one{
      children = [
                  #one_for_one{
                     id = first 
                    },
                  #one_for_one{
                                id = some_sup,
                                registered = some_sup,
                                children = [
                                            #worker{
                                               id = some,
                                               restart = permanent,
                                               start_func = {?MODULE, some_start_link, [some]}
                                              }
                                           ]
                              }
                 ]
      
     }.

some_start_link(_Name) ->
    {ok, spawn_link(fun() -> receive nothing -> ok end end)}.
    
