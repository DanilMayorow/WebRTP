%%%-------------------------------------------------------------------
%% @doc webrtp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(webrtp_sup).

-behaviour(supervisor).

-export([start_link/0, start_listener/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, ListenSock} = gen_tcp:listen(8080, [binary, {active, false}, {packet, 0}, {reuseaddr, true}]),
    spawn_link(fun initial_listeners/0),

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 60,
                 period => 3600},
    ChildSpecs = [#{id => socket,
                   start => {server, start_link, [ListenSock]},
                   restart => temporary,
                   shutdown => 1000,
                   type => worker,
                   modules => [server]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

start_listener() ->
    supervisor:start_child(?MODULE, []).

initial_listeners() ->
    [start_listener() || _ <- lists:seq(1, 5)],
    ok.