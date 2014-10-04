-module(bw_spider_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(SpiderModule, PipeLineModules) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [{SpiderModule, PipeLineModules}]).

init({SpiderModule, PipeLineModules}) ->
    GenSpiderSup = {gen_spider_sup, {gen_spider_sup, start_link, []},
	       permanent, brutal_kill, supervisor, [gen_spider]},
    SpiderManager = {bw_spider_manager, {bw_spider_manager, start_link, [SpiderModule, PipeLineModules]},
		     permanent, brutal_kill, worker, [bw_spider_manager]},
    Children = [GenSpiderSup, SpiderManager],
    RestartStrategy = {one_for_all, 10, 5 * 3600},
    {ok, {RestartStrategy, Children}}.
    
     
	       
% bw_spider_sup.erl:14: The inferred return type of init/1 
% ({'ok',{'one_for_all',10,18000},
%   [{'bw_spider_manager',{_,_,_},'permanent','brutal_kill','worker',[any(),...]} | {'gen_spider_sup',{_,_,_},'permanent','brutal_kill','supervisor',[any(),...]},...]}) 
% has nothing in common with 
% 'ignore' | 
% {'ok',{
%    {'one_for_all',non_neg_integer(),non_neg_integer()} | {'one_for_one',non_neg_integer(),non_neg_integer()} | {'rest_for_one',non_neg_integer(),non_neg_integer()} | {'simple_one_for_one',non_neg_integer(),non_neg_integer()},
%    [{_,{atom() | tuple(),atom(),'undefined' | [any()]},'permanent' | 'temporary' | 'transient','brutal_kill' | 'infinity' | non_neg_integer(),'supervisor' | 'worker','dynamic' | [atom() | tuple()]}]}},
%  which is the expected return type for the callback of supervisor behaviour
