-module(bw_spider_manager).

-export([start/2,
	 start_link/2,
	 init/2
	]).

start(SpiderModule, PipeLineModules) ->
    Pid = spawn(?MODULE, init, [SpiderModule, PipeLineModules]),
    {ok, Pid}.

start_link(SpiderModule, PipeLineModules) ->
    Pid = spawn_link(?MODULE, init, [SpiderModule, PipeLineModules]),
    {ok, Pid}.

-spec init(module(), [module()]) ->
    ok.
init(SpiderModule, PipeLineModules) ->
    UnvisitedUrls = gen_spider:start_urls(SpiderModule),
    VisitedUrls = [],
    MaxWorkers = gen_spider:max_workers(SpiderModule),
    [ gen_spider_sup:start_child(SpiderModule, self()) || _ <- lists:seq(1, MaxWorkers) ],
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    IdleWorkers = get_workers(MaxWorkers),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    BusyWorkers = [],
    {IdleWorkers2, BusyWorkers2, UnvisitedUrls2, VisitedUrls2} = assign_urls(IdleWorkers, BusyWorkers,  UnvisitedUrls, VisitedUrls),
    manager_loop(IdleWorkers2, BusyWorkers2, UnvisitedUrls2, VisitedUrls2, PipeLineModules, MaxWorkers).
    % manager_loop(IdleWorkers, BusyWorkers, UnvisitedUrls, VisitedUrls, PipeLineModules, MaxWorkers).


get_workers(MaxWorkers) ->
    get_workers(MaxWorkers, []).

get_workers(0, Workers) ->
    Workers;
get_workers(MaxWorkers, Workers) ->
    receive 
	{new_worker, NewWorker} ->
	    get_workers(MaxWorkers -1, [NewWorker|Workers])
    end.
    

-spec manager_loop(blackwidow:workers(), blackwidow:workers(), blackwidow:urls(), blackwidow:urls(), [module()], integer()) ->
    ok.
manager_loop(IdleWorkers, BusyWorkers, UnvisitedUrls, VisitedUrls, PipeLineModules, MaxWorkers) ->

    % {IdleWorkers2, BusyWorkers2, UnvisitedUrls2, VisitedUrls2} = assign_urls(IdleWorkers, BusyWorkers,  UnvisitedUrls, VisitedUrls),

    case {length(IdleWorkers), UnvisitedUrls} of
	{MaxWorkers, []} ->
	    io:format("~n[CRAWL FINISHED]~n~nNumber of urls crawled: ~p ~n", [length(VisitedUrls)]),
	    io:get_line("_____NEXT"),
	    ok;
	_ ->
	    receive
		{Worker, {{result, Result}, {urls, Urls}}} ->
		    io:format("~n[ RESULT ] ~p~n", [Worker]),
		    process_result(Result, PipeLineModules),
		    Urls2 = remove_duplicate_urls(lists:usort(Urls), VisitedUrls),
		    {IdleWorkers2, BusyWorkers2, UnvisitedUrls2, VisitedUrls2} = 
			assign_urls(IdleWorkers ++ [Worker], BusyWorkers,  UnvisitedUrls ++ Urls2, VisitedUrls),
		    io:format("Urls\t Urls2\t UnvisitedUrls\t UnvisitedUrls2\t VisitedUrls\t VisitedUrls2 ~n"),
		    io:format("~p~n", [{Urls, Urls2, UnvisitedUrls, UnvisitedUrls2, VisitedUrls, VisitedUrls2}]),
		    manager_loop(IdleWorkers2, BusyWorkers2, UnvisitedUrls2, VisitedUrls2, PipeLineModules, MaxWorkers);
		{new_worker, NewWorker} ->
		    io:format("~n[ WORKER ] ~p~n", [NewWorker]),
		    {IdleWorkers2, BusyWorkers2, UnvisitedUrls2, VisitedUrls2} = 
			assign_urls(IdleWorkers ++ [NewWorker], BusyWorkers,  UnvisitedUrls, VisitedUrls),
		    io:format("UnvisitedUrls\t UnvisitedUrls2\t VisitedUrls\t VisitedUrls2 ~n"),
		    io:format("~p~n", [{UnvisitedUrls, UnvisitedUrls2, VisitedUrls, VisitedUrls2}]),

		    manager_loop(IdleWorkers2, BusyWorkers2, VisitedUrls2, UnvisitedUrls2, PipeLineModules, MaxWorkers)
	    end
    end.

-spec assign_url(blackwidow:worker(), blackwidow:url()) ->
    {blackwidow:worker(), {assign_url, blackwidow:url()}}.
assign_url(Worker, Url) ->
    io:format("~n[ ASSIGNED URL ~p TO ~p ]~n", [Url, Worker]),
    Worker ! {self(), {assign_url, Url}}.

-spec assign_urls(blackwidow:workers(), blackwidow:workers(), blackwidow:urls(), blackwidow:urls()) ->
    {blackwidow:workers(), blackwidow:workers(), blackwidow:urls(), blackwidow:urls()}.
assign_urls(IdleWorkers, BusyWorkers, [], VisitedUrls) ->
    {IdleWorkers, BusyWorkers, [], VisitedUrls};
assign_urls([], BusyWorkers, UnvisitedUrls, VisitedUrls) ->
    {[], BusyWorkers, UnvisitedUrls, VisitedUrls};
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
