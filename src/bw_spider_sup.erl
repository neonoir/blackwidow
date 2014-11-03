%% The MIT License
%%
%% Copyright (c) 2014 Kamyar Navidan <kamyar.n@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

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
