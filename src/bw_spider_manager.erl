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
    NewUrls = gen_spider:start_urls(SpiderModule),
    MaxWorkers = gen_spider:max_workers(SpiderModule),
    [ gen_spider_sup:start_child(SpiderModule, self()) || _ <- lists:seq(1, MaxWorkers) ],
    IdleWorkers = get_workers(MaxWorkers),
    {IdleWorkers2, BusyWorkers2, NewUrls2, OldUrls2} = assign_urls(IdleWorkers, [],  NewUrls, []),
    manager_loop(IdleWorkers2, BusyWorkers2, NewUrls2, OldUrls2, PipeLineModules, MaxWorkers).

-spec get_workers(integer()) ->
    blackwidow:workers().    
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
manager_loop(IdleWorkers, BusyWorkers, NewUrls, OldUrls, PipeLineModules, MaxWorkers) ->
    case {length(IdleWorkers), NewUrls} of
	{MaxWorkers, []} ->
	    io:format("~n[CRAWL FINISHED]~n~nNumber of urls crawled: ~p ~n", [length(OldUrls)]),
	    io:get_line("_____NEXT"),
	    ok;
	_ ->
	    receive
		{Worker, {{result, Result}, {urls, Urls}}} ->
		    io:format("~n[ RESULT ] ~p~n", [Worker]),
		    process_result(Result, PipeLineModules),
		    Urls2 = remove_duplicate_urls(Urls, OldUrls),
		    {IdleWorkers2, BusyWorkers2, NewUrls2, OldUrls2} = 
			assign_urls([Worker | IdleWorkers], lists:delete(Worker, BusyWorkers),  lists:usort(NewUrls ++ Urls2), OldUrls),
		    io:format("Urls\t Urls2\t NewUrls\t NewUrls2\t OldUrls\t OldUrls2 ~n"),
		    io:format("~p~n", [{Urls, Urls2, NewUrls, NewUrls2, OldUrls, OldUrls2}]),
		    manager_loop(IdleWorkers2, BusyWorkers2, NewUrls2, OldUrls2, PipeLineModules, MaxWorkers)
	    end
    end.

-spec assign_url(blackwidow:worker(), blackwidow:url()) ->
    {blackwidow:worker(), {assign_url, blackwidow:url()}}.
assign_url(Worker, Url) ->
    io:format("~n[ ASSIGNED URL ~p TO ~p ]~n", [Url, Worker]),
    Worker ! {self(), {assign_url, Url}}.

-spec assign_urls(blackwidow:workers(), blackwidow:workers(), blackwidow:urls(), blackwidow:urls()) ->
    {blackwidow:workers(), blackwidow:workers(), blackwidow:urls(), blackwidow:urls()}.
assign_urls(IdleWorkers, BusyWorkers, [], OldUrls) ->
    {IdleWorkers, BusyWorkers, [], OldUrls};
assign_urls([], BusyWorkers, NewUrls, OldUrls) ->
    {[], BusyWorkers, NewUrls, OldUrls};
assign_urls([Worker|IdleWorkers], BusyWorkers, [Url|NewUrls], OldUrls) ->
    assign_url(Worker, Url),
    assign_urls(IdleWorkers, [Worker | BusyWorkers], NewUrls, [ Url | OldUrls]).

-spec remove_duplicate_urls(blackwidow:urls(), blackwidow:urls()) ->
    [blackwidow:urls()].
remove_duplicate_urls(NewUrls, OldUrls) ->
    [ Url || Url <- NewUrls, not lists:member(Url, OldUrls) ].

process_result(Result, PipeLineModules) ->
    case Result of
	ok ->
	    ok;
	_ ->
	    spawn(gen_spider_pipeline, handle_process_result, [Result, PipeLineModules])
    end.
