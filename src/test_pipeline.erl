-module(test_pipeline).

-behaviour(gen_spider_pipeline).

-export([handle_process_result/1]).

handle_process_result(Result) ->
    io:format("~p~n", [Result]).
