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

-module(bw_spider_manager).

-export([start/2]).
-export([start_link/2]).

-type worker() :: blackwidow:worker().
-type workers() :: blackwidow:workers().
-type url() :: blackwidow:url().
-type urls() :: blackwidow:urls().

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec start(module(), [module()]) -> {ok, pid()}.
start(SpiderModule, PipeLineModules) ->
    Pid = spawn(fun() -> init(SpiderModule, PipeLineModules) end),
    {ok, Pid}.

-spec start_link(module(), [module()]) -> {ok, pid()}.
start_link(SpiderModule, PipeLineModules) ->
    Pid = spawn_link(fun() -> init(SpiderModule, PipeLineModules) end),
    {ok, Pid}.

%% -----------------------------------------------------------------------------
%% internal
%% -----------------------------------------------------------------------------
-spec init(module(), [module()]) -> ok.
init(SpiderModule, PipeLineModules) ->
    NewUrls = gen_spider:start_urls(SpiderModule),
    MaxWorkers = gen_spider:max_workers(SpiderModule),
    IdleWorkers = start_workers(SpiderModule, MaxWorkers),
    {IdleWorkers1, BusyWorkers1, NewUrls1, OldUrls1} =
        do_assign_urls(IdleWorkers, [], NewUrls, []),
    loop(IdleWorkers1, BusyWorkers1, NewUrls1, OldUrls1,
         PipeLineModules, MaxWorkers).

-spec start_workers(module(), non_neg_integer()) -> workers().
start_workers(SpiderModule, MaxWorkers) ->
    [gen_spider_sup:start_child(SpiderModule, self()) ||
        _ <- lists:seq(1, MaxWorkers)],
    get_workers(MaxWorkers).

-spec get_workers(non_neg_integer()) -> workers().
get_workers(MaxWorkers) ->
    get_workers(MaxWorkers, []).

-spec get_workers(non_neg_integer(), workers()) -> workers().
get_workers(0, Workers) ->
    Workers;
get_workers(MaxWorkers, Workers) ->
    receive
	{new_worker, NewWorker} ->
	    get_workers(MaxWorkers - 1, [NewWorker|Workers])
    end.

-spec loop(workers(), workers(), urls(), urls(), [module()], integer()) -> ok.
loop(IdleWorkers, _, [], OldUrls, _, MaxWorkers)
  when length(IdleWorkers) =:= MaxWorkers ->
    io:format("~n[CRAWL FINISHED]~n~nNumber of urls crawled: ~p ~n", [length(OldUrls)]),
    ok;
loop(IdleWorkers, BusyWorkers, NewUrls, OldUrls, PipeLineModules, MaxWorkers) ->
    receive
        {Worker, {{result, Result}, {urls, Urls}}} ->
            % io:format("~n[ RESULT ] ~p~n", [Worker]),
            process_result(Result, PipeLineModules),
            {IdleWorkers1, BusyWorkers1, NewUrls1, OldUrls1} =
                assign_urls(Worker, IdleWorkers, BusyWorkers, Urls, NewUrls, OldUrls),
            loop(IdleWorkers1, BusyWorkers1, NewUrls1, OldUrls1,
                 PipeLineModules, MaxWorkers)
    end.

-spec assign_url(worker(), url()) -> {worker(), {assign_url, url()}}.
assign_url(Worker, Url) ->
    % io:format("~n[ ASSIGNED URL ~p TO ~p ]~n", [Url, Worker]),
    Worker ! {self(), {assign_url, Url}}.

-spec assign_urls(worker(), workers(), workers(), urls(), urls(), urls()) ->
                         {workers(), workers(), urls(), urls()}.
assign_urls(Worker, IdleWorkers, BusyWorkers, Urls, NewUrls, OldUrls) ->
    IdleWorkers1 = [Worker | IdleWorkers],
    BusyWorkers1 = lists:delete(Worker, BusyWorkers),
    Urls1 = remove_duplicate_urls(Urls, OldUrls),
    NewUrls1 = lists:usort(NewUrls ++ Urls1),
    do_assign_urls(IdleWorkers1, BusyWorkers1, NewUrls1, OldUrls).

-spec do_assign_urls(workers(), workers(), urls(), urls()) ->
                            {workers(), workers(), urls(), urls()}.
do_assign_urls(IdleWorkers, BusyWorkers, [], OldUrls) ->
    {IdleWorkers, BusyWorkers, [], OldUrls};
do_assign_urls([], BusyWorkers, NewUrls, OldUrls) ->
    {[], BusyWorkers, NewUrls, OldUrls};
do_assign_urls([Worker|IdleWorkers], BusyWorkers, [Url|NewUrls], OldUrls) ->
    assign_url(Worker, Url),
    do_assign_urls(IdleWorkers, [Worker | BusyWorkers], NewUrls, [Url | OldUrls]).

-spec remove_duplicate_urls(urls(), urls()) -> urls().
remove_duplicate_urls(NewUrls, OldUrls) ->
    [Url || Url <- NewUrls, not lists:member(Url, OldUrls)].

-spec process_result(term(), [module()]) -> ok.
process_result(ok, _) ->
    ok;
process_result(Result, PipeLineModules) ->
    spawn(gen_spider_pipeline, handle_process_result, [Result, PipeLineModules]),
    ok.
