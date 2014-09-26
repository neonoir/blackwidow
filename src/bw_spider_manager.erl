-module(bw_spider_manager).


-spec init(module(), [module()]) ->
    ok.
init(SpiderModule, PipeLineModules) ->
    StartUrls = gen_spider:start_urls(SpiderModule),
    Domain = gen_spider:domain(SpiderModule),
    MaxWorkers = gen_spider:max_workers(SpiderModule),
    Workers = [ spawn_worker(SpiderModule) || _ <- lists:seq(1, MaxWorkers) ],
    ok = assign_urls(Workers, Workers, StartUrls),
    manager_loop(Workers,Domain,StartUrls, PipeLineModules).

spawn_worker(SpiderModule) ->
    gen_spider:start(SpiderModule).

assign_url() ->
    Worker ! {self(), {assign_url, Url}}.
   
-spec assign_urls(workers(), workers(), urls()).
assign_urls(_, _, []) ->
    ok;
assign_urls([], OrigWorkers, Urls)->
    assign_urls(OrigWorkers, OrigWorkers, Urls);
assign_urls([Worker | Workers], OrigWorkers, [Url | Urls]) ->
    assign_url(Worker, Url),
    assign_urls(Workers, OrigWorkers, Urls).

manager_loop(Workers,Domain,StartUrls, PipeLineModules) ->
    Urls = [ {Url, Visited} || {Url, Visited} <- StartUrls, Visited == not_visited ],
    assign_urls(Workers, Workers, Urls),
    VisitedUrls = [ {Url, visited} || {Url, _} <- StartUrls ],
    receive 
	{Pid, {result, Result}} ->
	    process_result(Result, PipeLineModules),
	    manager_loop(Workers, Domain, VisitedUrls, PipeLineModules);
	{Pid, {urls, Urls}} ->
	    UrlList2 = remove_duplicate_urls(Urls, VisitedUrls),
	    manager_loop(Workers, Domain, UrlList2, PipeLineModules)
    end.

check_url(Url) ->
     ok.

remove_duplicate_urls(NewUrls, VisitedUrls) ->
    [ {Url, not_visited} || {Url, _} <- NewUrls, lists:member({Url, visited}, VisitedUrls), check_url(Url) ] ++ VisitedUrls.

isin_domain(Url, Domain) ->
    {_, _, Host, _, _, _} = http_uri:parse(Url),
    Host == Domain.

process_result(Result, PipeLineModules) ->
    spawn(gen_spider_pipeline, handle_process_result, [Result, PipeLineModules]).

  
