-module(gen_spider_sup).

-behaviour(supervisor).

-export([start_link/0,
	 start_child/2
	]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(SpiderModule, ManagerPid) ->
    supervisor:start_child(?SERVER, [SpiderModule, ManagerPid]).
    

init([]) ->
    Element = {gen_spider, {gen_spider, start_link, []},
	       permanent, brutal_kill, worker, [gen_spider]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
    
     
	       
