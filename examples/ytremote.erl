%% @author Tobias Rodaebel
%% @doc Karajan Event Handler for the YouTube Remote Demo

-module(ytremote).

-behaviour(gen_event).

%% API 
-export([start/1, stop/0]).

%% Callbacks of the gen_event behaviour
-export([init/1, code_change/3, handle_event/2, handle_call/2, handle_info/2,
         terminate/2]).

-define(EVENT_MANAGER, tosca_event).

-define(WEBSOCKET_SERVER, {websocket_server, websocket@localhost}).

%% @doc Starts the handler.
%% @spec start(Options) -> already_started | ok
start(Options) ->
    case lists:member(?MODULE, gen_event:which_handlers(?EVENT_MANAGER)) of
        true  ->
            already_started;
        false ->
            gen_event:add_sup_handler(?EVENT_MANAGER, ?MODULE, Options)
    end.

%% @doc Stops the handler.
%% @spec stop() -> ok
stop() ->
    gen_event:delete_handler(?EVENT_MANAGER, ?MODULE, []).

%% @doc Initializes the handler.
%% @spec init(Args) -> {ok, Clients}
init(_Args) ->
    Clients = sets:new(),
    {ok, Clients}.

%% @private
%% @doc Broadcasts OSC message to given clients.
%% @spec broadcast_message(Msg, Clients, Socket) -> any()
broadcast_message(Msg, Clients, Socket) ->
    {ok, Port} = application:get_env(tosca, outgoing_port),
    Send = fun(Addr, Acc) ->
        gen_udp:send(Socket, Addr, Port, osc_lib:encode(Msg)),
        Acc
    end,
    sets:fold(Send, [], Clients).

%% @private
%% @doc Handles events.
%% @spec handle_event(Event, Clients) -> {ok, Clients}
handle_event({{_When,[_,"fader1"],Value},Socket,Ip}, Clients)->
    case sets:is_element(Ip, Clients) of
        true ->
            gen_server:cast(?WEBSOCKET_SERVER, {offset, Value}),
            Peers = sets:subtract(Clients, sets:from_list([Ip])),
            broadcast_message({message,"/1/fader1",Value}, Peers, Socket);
        false ->
            ok
    end,
    {ok, Clients};
handle_event({{_When,[_,"toggle1"],[1.0]},_Socket,Ip}, Clients)->
    {ok, sets:add_element(Ip, Clients)};
handle_event({{_When,[_,"toggle1"],[0.0]},_Socket,Ip}, Clients)->
    {ok, sets:del_element(Ip, Clients)};
handle_event({{_When,[_,"toggle2"],Value},Socket,Ip}, Clients)->
    case sets:is_element(Ip, Clients) of
        true ->
            gen_server:cast(?WEBSOCKET_SERVER, {play, Value}),
            Peers = sets:subtract(Clients, sets:from_list([Ip])),
            broadcast_message({message,"/1/toggle2",Value}, Peers, Socket);
        false ->
            ok
    end,
    {ok, Clients};
handle_event({{message,"/1/fader1",Value}, Socket}, Clients)->
    broadcast_message({message,"/1/fader1",Value}, Clients, Socket),
    {ok, Clients};
handle_event(Event, Clients)->
    error_logger:info_msg("~p ~p~n", [self(), Event]),
    {ok, Clients}.

%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, Clients) -> {ok, {error, bad_query}, Clients}
handle_call(_Request, Clients) ->
    {ok, {error, bad_request}, Clients}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, Clients) -> {noreply, Clients}
handle_info(_Info, Clients) ->
    {noreply, Clients}.

%% @private
%% @doc Performs cleanup on termination.
%% @spec terminate(Reason, State) -> ok
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, Library, Extra) -> {ok, Library}
code_change(_OldVsn, Library, _Extra) ->
    {ok, Library}.
