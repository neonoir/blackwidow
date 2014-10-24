-module(test_crawler).

-include("blackwidow.hrl").

-behaviour(gen_spider).

%% gen_spider callbacks
-export([
	 spider_name/0,
	 domain/0,
	 start_urls/0,
	 max_workers/0,
	 process_response/1
	]).

spider_name() ->
    "test_crawler".

domain() ->
    "example.com".

start_urls() ->
    [1003].

max_workers() ->
    4.
%
process_response(#bw_response{url=Url}) ->
    { {result, Url}, {urls, [ random:uniform(10) + 1000  || _ <- lists:seq(1,10) ]} }.

