-module(bw_spider_manager).

-export([start/2,
	 start_link/2,
	 init/2
	]).

start(SpiderModule, PipeLineModules) ->
    spawn(?MODULE, init, [SpiderModule, PipeLineModules]).    

start_link(SpiderModule, PipeLineModules) ->
    spawn_link(?MODULE, init, [SpiderModule, PipeLineModules]).

-spec init(module(), [module()]) ->
    ok.
init(SpiderModule, PipeLineModules) ->
    UnvisitedUrls = gen_spider:start_urls(SpiderModule),
    VisitedUrls = [],
    MaxWorkers = gen_spider:max_workers(SpiderModule),
    [ gen_spider_sup:start_child(self()) || _ <- lists:seq(1, MaxWorkers) ],
    IdleWorkers = [],
    BusyWorkers = [],
    manager_loop(IdleWorkers, BusyWorkers, UnvisitedUrls, VisitedUrls, PipeLineModules, MaxWorkers).

-spec manager_loop(blackwidow:workers(), blackwidow:workers(), blackwidow:urls(), blackwidow:urls(), [module()], integer()) ->
    ok.
manager_loop(IdleWorkers, BusyWorkers, UnvisitedUrls, VisitedUrls, PipeLineModules, MaxWorkers) ->
    Pid = self(),
    {IdleWorkers2, BusyWorkers2, UnvisitedUrls2, VisitedUrls2} = assign_urls(IdleWorkers, BusyWorkers,  UnvisitedUrls, VisitedUrls),

    case {length(IdleWorkers), UnvisitedUrls} of
	{MaxWorkers, []} ->
	    ok;
	_ -> 
	    receive 
		{Worker, {{result, Result}, {urls, Urls}}} ->
		    ok = process_result(Result, PipeLineModules),
		    Urls2 = remove_duplicate_urls(Urls, VisitedUrls),
		    manager_loop(IdleWorkers2 ++ [Worker], BusyWorkers2, UnvisitedUrls2 ++ Urls2, VisitedUrls2, PipeLineModules, MaxWorkers);
		{Pid, {new_worker, NewWorkerPid}} ->
		    manager_loop(IdleWorkers2 ++ [NewWorkerPid], BusyWorkers2, VisitedUrls2, UnvisitedUrls2, PipeLineModules, MaxWorkers)
	    end
    end.

-spec assign_url(blackwidow:worker(), blackwidow:url()) ->
    {blackwidow:worker(), {assign_url, blackwidow:url()}}.
assign_url(Worker, Url) ->
    Worker ! {self(), {assign_url, Url}}.

-spec assign_urls(blackwidow:workers(), blackwidow:workers(), blackwidow:urls(), blackwidow:urls()) -> 
    {blackwidow:workers(), blackwidow:workers(), blackwidow:urls(), blackwidow:urls()}.

assign_urls([], BusyWorkers, UnvisitedUrls, VisitedUrls) ->
    {[], BusyWorkers, UnvisitedUrls, VisitedUrls};

assign_urls(IdleWorkers, BusyWorkers, [], VisitedUrls) ->
    {IdleWorkers, BusyWorkers, [], VisitedUrls};

assign_urls([Worker|IdleWorkers], BusyWorkers, [Url|UnvisitedUrls], VisitedUrls) ->
    assign_url(Worker, Url),
    assign_urls(IdleWorkers, BusyWorkers ++ [Worker], UnvisitedUrls, VisitedUrls ++ [Url]).

-spec remove_duplicate_urls(blackwidow:urls(), blackwidow:urls()) ->
    [blackwidow:urls()].
remove_duplicate_urls(NewUrls, VisitedUrls) ->
    case NewUrls of 
	[] ->
	    [];
	_ ->
	    [ Url || Url <- NewUrls, not lists:member(Url, VisitedUrls) ]
    end.


process_result(Result, PipeLineModules) ->
    case Result of
	ok ->
	    ok;
	_ ->
	    spawn(gen_spider_pipeline, handle_process_result, [Result, PipeLineModules])
    end.

