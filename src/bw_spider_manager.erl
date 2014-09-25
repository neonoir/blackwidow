-module(bw_spider_manager).

-type worker() :: pid().
-type workers() :: [worker()].
-type url() :: {string(), visited|not_visited}.
-type urls() :: [url()].



init(StartUrlList, Domain, Config, MFA, MaxWorkers) ->
    Workers = [ spawn_worker(MFA) || _ <- lists:seq(Maxworkers) ],
    ok = initial_url_assign(Workers, Workers, StartUrlList),


assign_url() ->
    gen_spider_worker:assign_url(Worker, Url).
   
-spec assign_urls(workers(), workers(), urls()).
assign_urls(_, _, []) ->
    ok;
assign_urls([], OrigWorkers, Urls)->
    assign_urls(OrigWorkers, OrigWorkers, Urls);
assign_urls([Worker | Workers], OrigWorkers, [Url | Urls]) ->
    assign_url(Worker, Url),
    assign_urls(Workers, OrigWorkers, Urls).

spawn_worker(MFA) ->
    gen_spider_worker:start(MFA).

manager_loop(Workers,Domain,StartUrlList) ->
    Urls = [ {Url, Visited} || {Url, Visited} <- StartUrlList, Visited == not_visited ],
    assign_urls(Workers, Workers, Urls),
    VisitedUrls = [ {Url, visited} || {Url, _} <- StartUrlList ],
    receive 
	{Pid, {result, Result}} ->
	    process_result(Result),
	    manager_loop(Workers, Domain, VisitedUrls);
	{Pid, {urls, Urls}} ->
	    UrlList2 = remove_duplicate_urls(Urls, VisitedUrls),
	    manager_loop(Workers, Domain, UrlList2)
    end.

check_url(Url) ->
     ok.

remove_duplicate_urls(NewUrls, VisitedUrls) ->
    [ {Url, not_visited} || {Url, _} <- NewUrls, lists:member({Url, visited}, VisitedUrls), check_url(Url) ] ++ VisitedUrls.

isin_domain(Url, Domain) ->
    {_, _, Host, _, _, _} = http_uri:parse(Url),
    Host == Domain.

process_result(Result) ->
    ok.

  
