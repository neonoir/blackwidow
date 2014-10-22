-module(bw_spider_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(SpiderModule, PipeLineModules) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {SpiderModule, PipeLineModules}).

init({SpiderModule, PipeLineModules}) ->
    GenSpiderSup = {gen_spider_sup, {gen_spider_sup, start_link, []},
	       permanent, brutal_kill, supervisor, [gen_spider]},
    SpiderManager = {bw_spider_manager, {bw_spider_manager, start_link, [SpiderModule, PipeLineModules]},
		     transient, brutal_kill, worker, [bw_spider_manager]},
    Children = [GenSpiderSup, SpiderManager],
    RestartStrategy = {one_for_one, 10, 5 * 3600},
    {ok, {RestartStrategy, Children}}.
