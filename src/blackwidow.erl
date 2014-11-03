-module(blackwidow).

-type worker() :: pid().
-type workers() :: [worker()].
-type url() :: string().
-type urls() :: [url()].

-include("blackwidow.hrl").

-type response() :: #bw_response{}.

-export_type([worker/0, workers/0, url/0, urls/0, response/0]).

-export([
	 start_spider/2
	]).

start_spider(SpiderModule, [PipeLineModules]) ->
    ensure_deps_started(),
    bw_sup:start_child(SpiderModule, PipeLineModules).




-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% -----------------------------------------------------------------------------
%% ensure dependencies are started
%% -----------------------------------------------------------------------------
-spec ensure_deps_started() -> ok.
ensure_deps_started() ->
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(cowlib),
    ensure_started(cowboy).
