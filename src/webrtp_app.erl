%%%-------------------------------------------------------------------
%% @doc webrtp public API
%% @end
%%%-------------------------------------------------------------------

-module(webrtp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    webrtp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
