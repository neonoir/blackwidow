%%%-------------------------------------------------------------------
%%% File    : bw_sup.erl
%%% Author  : Kamyar Navidan <kamyar.n@gmail.com>
%%% Description : 
%%%
%%% Created :  2 Nov 2014 by Kamyar Navidan <kamyar.n@gmail.com>
%%%-------------------------------------------------------------------
-module(bw_sup).

-behaviour(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 start_child/2
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(SpiderModule, PipeLineModules) ->
    supervisor:start_child(?SERVER, [SpiderModule, [PipeLineModules]]).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([]) ->
    Child = {bw_spider_sup, {bw_spider_sup, start_link, []},
	       permanent, brutal_kill, supervisor, [bw_spider_sup]},
    Children = [Child],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
