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
