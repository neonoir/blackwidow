-module(gen_spider).

-include("blackwidow.hrl").

-export([start_link/2,
	 start/2
	]).

-export([start_urls/1,
	 max_workers/1,
	 domain/1,
	 process_response/2,
	 spider_name/1]).

-export([worker_loop/2]).


-callback spider_name() ->
    string().
-callback domain(Module :: module()) ->
    string().
-callback start_urls(Module :: module()) ->
    [blackwidow:url()].
-callback max_workers(Module :: module()) ->
    integer().
% -callback is_url_valid(Url :: blackwidow:url()) ->
%     boolean().
-callback process_response(Response :: blackwidow:response()) ->
    {pid(), 
     { 
	  {result, Result :: term()}, 
	  {urls, Url :: blackwidow:urls()} 
	 } }.


start_link(Module, ManagerPid) ->
    Pid = spawn_link(?MODULE, worker_loop, [Module, ManagerPid]),
    ManagerPid ! {new_worker, Pid},
    {ok, Pid}.

start(Module, ManagerPid) ->
    Pid = spawn(?MODULE, worker_loop, [Module, ManagerPid]),
    ManagerPid ! {new_worker, Pid},
    {ok, Pid}.		  

worker_loop(Module, ManagerPid) ->
    
    receive
	{ManagerPid, {assign_url, Url}} ->
	    Response = fetch_url(Url),
	    ManagerPid ! {self(), process_response(Module, Response)}
    end.

fetch_url(Url) ->
    #bw_response{}.


% extract_urls(Response) ->
%     ok.

% is_url_valid(Module, Url) ->
%     Module:is_url_valid(Url).
-spec process_response(module(), blackwidow:response()) ->
    {{result, term()}, {urls, blackwidow:urls()}}.
process_response(Module, Response) ->
    Module:process_response(Response).

start_urls(Module) ->
    Module:start_urls().

domain(Module) ->
    Module:domain().

max_workers(Module) ->
    Module:max_workers().

spider_name(Module) ->
    Module:spider_name().


% isin_domain(Url, Domain) ->
%     {_, _, Host, _, _, _} = http_uri:parse(Url),
%     Host == Domain.
