-module(gen_spider_pipeline).

-callback process_result(Result :: term()) ->
    term().

handle_process_result(Result, PipeLineModules) ->
    F = fun(Module, ResultIn) -> Module:process_result(ResultIn) end,
    lists:foldl(F, Result, PipeLineModules).
    
    
