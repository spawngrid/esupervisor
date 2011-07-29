-module(esupervisor).
-behaviour(supervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

-export([start_link/2, start_link/3]).
-export([behaviour_info/1]).
-export([start_sup/2]).

%% Supervisor callbacks
-export([init/1]).

-export_type([supervisable/0]).

-type supervisor() :: #supervisor{} | #one_for_one{} | 
                      #one_for_all{} | #rest_for_one{} |
                      #simple_one_for_one{}.

-type child() :: #child{} | #worker{}.

-type supervisable() :: child() | supervisor().

behaviour_info(callbacks) ->
    [{init,1}];
behaviour_info(_) ->
    undefined.

-include_lib("eunit/include/eunit.hrl").

init({spec, Spec}) -> 
    init_result(Spec); 
init({Module, Args}) ->                  
    init_result(Module:init(Args)).

-spec start_link(module(), term()) -> supervisor:start_link_ret().
                        
start_link(Module, Args) ->
    supervisor:start_link(?MODULE, {Module, Args}).

start_link(SupName, Module, Args) ->
    supervisor:start_link(SupName, ?MODULE, {Module, Args}).

-spec start_sup(atom() | undefined, supervisor()) -> {ok, pid()}.

start_sup(false, SupervisorSpec) ->
    supervisor:start_link(?MODULE, {spec, SupervisorSpec});
start_sup(Reg, SupervisorSpec) ->       
    supervisor:start_link({local, Reg}, ?MODULE, {spec, SupervisorSpec}).

%% Internal functions
init_result(ignore) ->
    ignore;
init_result(Result) ->
    {ok, spec(Result)}.

specs([]) ->
    [];
specs([Spec|Rest]) ->
    [spec(Spec)|specs(Rest)].

spec(#worker{
        id = Id,
        start_func = StartFunc,
        restart = Restart,
        shutdown = Shutdown,
        modules = Modules
       }) ->
    spec(#child{
            type = worker,
            id = Id,
            start_func = StartFunc,
            restart = Restart,
            shutdown = Shutdown,
            modules = Modules
           });
spec(#child{
        id = Id,
        start_func = undefined
       } = Child) ->
    spec(Child#child{ start_func = {Id, start_link, []}});
spec(#child{
        id = Id,
        modules = undefined
       } = Child) ->
    spec(Child#child{ modules = [Id] });
spec(#child{
        id = Id,
        start_func = StartFunc,
        restart = Restart,
        shutdown = Shutdown,
        modules = Modules,
        type = Type
       }) ->
    Child = {Id, StartFunc, Restart, Shutdown, Type, Modules},
    ok = supervisor:check_childspecs([Child]),
    Child;

spec(#one_for_one{
        id = Id,
        max_restarts = MaxRT,
        children = Children
       }) ->
    spec(#supervisor{ id = Id,
                      restart_strategy = one_for_one,
                      max_restarts = MaxRT,
                      children = Children });

spec(#one_for_all{
        id = Id,
        max_restarts = MaxRT,
        children = Children
       }) ->
    spec(#supervisor{ id = Id,
                      restart_strategy = one_for_all,
                      max_restarts = MaxRT,
                      children = Children });

spec(#rest_for_one{
        id = Id,
        max_restarts = MaxRT,
        children = Children
       }) ->
    spec(#supervisor{ id = Id,
                      restart_strategy = rest_for_one,
                      max_restarts = MaxRT,
                      children = Children });

spec(#simple_one_for_one{
        id = Id,
        max_restarts = MaxRT,
        children = Children
       }) ->
    spec(#supervisor{ id = Id,
                      restart_strategy = simple_one_for_one,
                      max_restarts = MaxRT,
                      children = Children });

spec(#supervisor{
        id = _Id,
        restart_strategy = Restart,
        max_restarts = {MaxR, MaxT},
        children = Children
       }) ->
    {{Restart, MaxR, MaxT}, specs(maybe_convert_supervisors(Children))}.

    
maybe_convert_supervisors([]) ->
    [];
maybe_convert_supervisors([Spec|Specs]) ->
    [maybe_convert_supervisor(Spec)|maybe_convert_supervisors(Specs)].

maybe_convert_supervisor(#one_for_one{ 
                            id = Id,
                            registered = Reg, restart = Restart,
                            shutdown = Shutdown
                           } = Sup) ->
    #child{
            id = Id,
            type = supervisor,
            start_func = {?MODULE, start_sup, [Reg, Sup]},
            restart = Restart,
            shutdown = Shutdown,
            modules = [?MODULE]
          };

maybe_convert_supervisor(#one_for_all{ 
                            id = Id,
                            registered = Reg, restart = Restart,
                            shutdown = Shutdown
                           } = Sup) ->
    #child{
            id = Id,
            type = supervisor,
            start_func = {?MODULE, start_sup, [Reg, Sup]},
            restart = Restart,
            shutdown = Shutdown,
            modules = [?MODULE]
          };

maybe_convert_supervisor(#rest_for_one{ 
                            id = Id,
                            registered = Reg, restart = Restart,
                            shutdown = Shutdown
                           } = Sup) ->
    #child{
            id = Id,
            type = supervisor,
            start_func = {?MODULE, start_sup, [Reg, Sup]},
            restart = Restart,
            shutdown = Shutdown,
            modules = [?MODULE]
          };

maybe_convert_supervisor(#simple_one_for_one{ 
                            id = Id,
                            registered = Reg, restart = Restart,
                            shutdown = Shutdown
                           } = Sup) ->
    #child{
            id = Id,
            type = supervisor,
            start_func = {?MODULE, start_sup, [Reg, Sup]},
            restart = Restart,
            shutdown = Shutdown,
            modules = [?MODULE]
          };

maybe_convert_supervisor(#supervisor{ 
                            id = Id,
                            registered = Reg, restart = Restart,
                            shutdown = Shutdown
                           } = Sup) ->
    #child{
            id = Id,
            type = supervisor,
            start_func = {?MODULE, start_sup, [Reg, Sup]},
            restart = Restart,
            shutdown = Shutdown,
            modules = [?MODULE]
          };


maybe_convert_supervisor(Other) ->
    Other.
