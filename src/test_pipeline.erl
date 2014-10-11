-module(test_pipeline).

-behaviour(gen_spider_pipeline).

-export([process_result/1]).

process_result(Result) ->
    io:format("~p~n", [Result]).
