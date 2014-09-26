-module(gen_spider).

-callback is_url_valid(Url :: blackwidow:url()) ->
    boolean().
-callback process_response(Response :: blackwidow:response()) ->
    {url, Url :: blackwidow:url()} | {result, Result :: term()} | ok.
-callback start_urls(Module :: module()) ->
    [blackwidow:url()].
-callback domain(Module :: module()) ->
    string().
-callback max_workers(Module :: module()) ->
    integer().


start(Module) ->
    spawn(?MODULE, worker_loop, [Module]).

worker_loop(Module) ->
    receive
	{From, {assign_url, Url}} ->
	    Response = fetch_url(Url),
	    From ! {urls, extract_urls(Response)},
	    From ! {results, process_response(Module, Response)}
    end.

fetch_url(Module, Url) ->
    case erlang:function_exported(Module, fetch_url, 1) of 
	true ->
	    Module:fetch_url(Url);
	_ ->
	    ok
    end.

extract_urls(Response) ->
    ok.

is_url_valid(Module, Url) ->
    Module:is_url_valid(Url).

process_response(Module, Response) ->
    Module:process_response(Response).

start_urls(Module) ->
    Module:start_urls().

domain(Module) ->
    Module:domain().

max_workers(Module) ->
    Module:max_workers().

	    
