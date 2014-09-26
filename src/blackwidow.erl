-module(blackwidow).

-type worker() :: pid().
-type workers() :: [worker()].
-type url() :: {string(), visited|not_visited}.
-type urls() :: [url()].

-include("blackwidow.hrl").

-type response() :: #bw_response{}.

-export_type([worker/0, workers/0, url/0, urls/0, response/0]).
