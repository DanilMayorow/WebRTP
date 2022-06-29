-module(server).
-behavior(gen_server).

-compile([{parse_transform, lager_transform}]).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket :: port(),
                accept_maps = maps:new() :: map(),
                packet :: binary()
        }).

%%% module API

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

%%% gen server API

init([Socket]) ->
    gen_server:cast(self(), listen),
    lager:info("Spawn server thread [~p]", [erlang:self()]),
    {ok, #state{socket = Socket}}.

handle_call(_Any, _From, State) ->
    {reply, ok, State}.


handle_cast(listen, State) ->
    ListenSocket = State#state.socket,
    {ok, AcceptSock} = gen_tcp:accept(ListenSocket),
    inet:setopts(AcceptSock, [{active, once}]),
    {noreply, State#state{socket=AcceptSock}};

handle_cast({receiverd, Packet}, State) ->
    webrtp_sup:start_listener(),
    AcceptSock = State#state.socket,
    Response = get_response(State, Packet),
    gen_tcp:send(AcceptSock, Response),
    lager:info("Session closed in [~p]", [erlang:self()]),
    gen_tcp:close(AcceptSock),
    {stop, normal, State}.


handle_info({tcp, _ClientSocket, Packet}, State) ->
    lager:info("Socket in [~p] got message: ~p", [erlang:self(), Packet]),
    NewState = State#state{packet = Packet},
    gen_server:cast(self(), {receiverd, Packet}),
    {noreply, NewState};

handle_info({tcp_closed, _ClientSocket}, State) ->
    AcceptSock = State#state.socket,
    gen_tcp:close(AcceptSock),
    lager:info("Session closed in [~p] and thread will be restart now", [erlang:self()]),
    webrtp_sup:start_listener(),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%% inner function

get_response(State, Packet) ->
    AcceptSock = State#state.socket,
    inet:setopts(AcceptSock, [{active, once}]),
    case webrtp:parse_packet(Packet) of
        {ok, get, Packet} -> webrtp:get_page(init);
        {ok, post, Phone, Text} -> webrtp:post(Phone, Text);
        {C, R, S} -> webrtp:get_error_page({C, R, S})
    end.