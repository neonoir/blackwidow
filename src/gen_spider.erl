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
-callback domain() ->
    string().
-callback start_urls() ->
    [blackwidow:url()].
-callback max_workers() ->
    integer().
-callback process_response(Response :: blackwidow:response()) ->
    {{result, Result :: term()}, {urls, Url :: blackwidow:urls()}}.


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
	    ManagerPid ! {self(), process_response(Module, Response)},
	    worker_loop(Module, ManagerPid)
    end.

fetch_url(Url) ->
    #bw_response{url=Url}.


% extract_urls(Response) ->
%     ok.

% is_url_valid(Module, Url) ->
%     Module:is_url_valid(Url).
-spec process_response(module(), blackwidow:response()) ->
    {{result, term()}, {urls, blackwidow:urls()}}.
process_response(Module, Response) ->
    Module:process_response(Response).

-spec start_urls(module()) ->
    blackwidow:urls().
start_urls(Module) ->
    Module:start_urls().

-spec domain(module()) ->
    string().
domain(Module) ->
    Module:domain().

-spec max_workers(module()) ->
    integer().
max_workers(Module) ->
    Module:max_workers().

-spec spider_name(module()) ->
    string().
spider_name(Module) ->
    Module:spider_name().


% isin_domain(Url, Domain) ->
%     {_, _, Host, _, _, _} = http_uri:parse(Url),
%     Host == Domain.
