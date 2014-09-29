-module(bw_spider_manager).

-export([start/2,
	 start_link/2
	]).

start_link(SpiderModule, PipeLineModules) ->
    spawn_link(?MODULE, init, [SpiderModule, PipeLineModules]).

start(SpiderModule, PipeLineModules) ->
    spawn(?MODULE, init, [SpiderModule, PipeLineModules]).    

-spec init(module(), [module()]) ->
    ok.
init(SpiderModule, PipeLineModules) ->
    UnvisitedUrls = gen_spider:start_urls(SpiderModule),
    VisitedUrls = [],
    Domain = gen_spider:domain(SpiderModule),
    MaxWorkers = gen_spider:max_workers(SpiderModule),
    [ gen_server_sup:start_child(self()) || _ <- lists:seq(1, MaxWorkers) ] ],
    IdleWorkers = [],
    BusyWorkers = [],
    empty_mailbox(),
    manager_loop(IdleWorkers, BusyWorkers, UnvisitedUrls, VisitedUrls, PipeLineModules).

manager_loop(IdleWorkers, BusyWorkers, UnvisitedUrls, VisitedUrls, PipeLineModules) ->

    {IdleWorkers2, NewBusyWorkers2, UnvisitedUrls2, VisitedUrls2} = assign_urls(IdleWorkers, BusyWorkers,  UnvisitedUrls, VisitedUrls),

    receive 
	{Worker, {result, Result}, {urls, Urls}} ->
	    process_result(Result, PipeLineModules),

	    Urls2 = remove_duplicate_urls(Urls, VisitedUrls),
	    manager_loop(IdleWorkers2 ++ [Worker], BusyWorkers2, UnvisitedUrls2 ++ Urls2, VisitedUrls2, PipeLineModules);
	{self(), {new_worker, NewWorkerPid}} ->
	    manager_loop(IdleWorkers2 ++ [NewWorkerPid], BusyWorkers2, VisitedUrls2, UnvisitedUrls2, PipeLineModules)
    end.

-spec assign_url(worker(), url()).
assign_url(Worker, Url) ->
    Worker ! {self(), {assign_url, Url}}.

-spec assign_urls(workers(), workers(), urls()).
assign_urls([], BusyWorkers, UnvisitedUrls, VisitedUrls)->
    {[], BusyWorkers, UnvisitedUrls, VisitedUrls};
assign_urls(IdleWorkers, BusyWorkers, [], VisitedUrls) ->
    {IdleWorkers, BusyWorkers, [], VisitedUrls};
assing_urls([Worker|IdleWorkers], BusyWorkers, [Url|UnvisitedUrls], VisitedUrls) ->
    assign_url(Worker, Url),
    assing_urls(IdleWorkers, BusyWorkers ++ [Worker], UnvisitedUrls, VisitedUrls ++ [Url]).

%% part of gen_spider callback module's logic. SHOULD BE MOVED TO gen_spider
check_url(Url) ->
     ok.

remove_duplicate_urls(NewUrls, VisitedUrls) ->
    [ Url || Url <- NewUrls, not lists:member(Url, VisitedUrls) ].

%% part of gen_spider callback module's logic. SHOULD BE MOVED TO gen_spider
isin_domain(Url, Domain) ->
    {_, _, Host, _, _, _} = http_uri:parse(Url),
    Host == Domain.

process_result(Result, PipeLineModules) ->
    spawn(gen_spider_pipeline, handle_process_result, [Result, PipeLineModules]).
