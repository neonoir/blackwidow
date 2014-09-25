-module(gen_spider).

-callback is_url_valid(Url :: string()) ->
    boolean().
-callback process_response(Response :: string()) ->
    {url, Url :: string()} | {result, Result :: term()} | ok.


start(Module) ->
    spawn(?MODULE, worker_loop, [Module]).
    

worker_loop(Module) ->
    receive
	{From, Url} ->
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



	    
