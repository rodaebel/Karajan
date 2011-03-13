%% @author Tobias Rodaebel
%% @doc Karajan OSC Server
%%
%% The server process listens for incoming UDP packets. It decodes received
%% data and fires events containing the OSC message.

-module(karajan_server).
-vsn("1.0.0").

-behaviour(gen_server).

%% Server API
-export([start_link/0]).

%% Callbacks of the gen_server behaviour
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-include("karajan.hrl").

-record(state, {socket=null}).

-define(SERVER, ?MODULE). 

%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
%% @doc Initializes the server.
%% @spec init(Args) -> {ok, State} | {stop, Reason}
init([]) ->
    {ok, Ip} = application:get_env(ip),
    {ok, Port} = application:get_env(incoming_port),
    {ok, RecBuf} = application:get_env(recbuf),
    Options = [binary, {ip, Ip}, {active, once}, {recbuf, RecBuf}],
    case gen_udp:open(Port, Options) of
    	{ok, Socket} ->
            error_logger:info_msg("~p Listening on port ~p~n", [self(), Port]),
	        {ok, #state{socket = Socket}};
	    {error, Reason} ->
	        error_logger:error_report({?MODULE, udp_open, Reason}),
	        {stop, {?MODULE, udp_open, Reason}}
    end.

%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, From, State) -> {noreply, State}
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handles cast messages.
%% @spec handle_cast(Msg, State) -> {noreply, State}
handle_cast({message, Address, Args}, State) ->
    error_logger:info_msg("~p ~p ~p~n", [self(), Address, Args]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State}
handle_info({udp, Socket, Ip, _Port, Packet}, State) ->
    inet:setopts(Socket, [{active, once}]),
    try osc_lib:decode(Packet) of
    	{message, Address, Args} ->
            Message = {immediately, string:tokens(Address, "/"), Args},
            gen_event:notify(karajan_event, {Message, Socket, Ip});
	    {bundle, When, Elements} ->
	        handle_bundle(Socket, Ip, When, Elements)
    catch
	    _:_ ->
	        error_logger:error_msg("~p Decoding OSC failed~n", [self()])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc Performs cleanup on termination.
%% @spec terminate(Reason, State) -> ok
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
    ok.

%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, Library, Extra) -> {ok, Library}
code_change(_OldVsn, Library, _Extra) ->
    {ok, Library}.

%% @private
%% @doc Handles OSC bundles.
%% @spec handle_bundle(Socket, Ip, When::time(), Elements::list()) -> any()
%%       Elements = [message() | bundle()]
%%       message() = {message, Address::string(), Args::[any()]}
%%       bundle() = {bundle, When::time(), [message() | bundle()]}
%%       time() = immediately | {time, Seconds::integer(), Fractions::integer()}
handle_bundle(_Socket, _Ip, _When, []) ->
    ok;
handle_bundle(Socket, Ip, When, [{message, Address, Args} | Rest]) ->
    Message = {When, string:tokens(Address, "/"), Args},
    gen_event:notify(karajan_event, {Message, Socket, Ip}),
    handle_bundle(Socket, Ip, When, Rest);
handle_bundle(Socket, Ip, When, [{bundle, InnerWhen, Elements} | Rest]) ->
    handle_bundle(Socket, Ip, InnerWhen, Elements),
    handle_bundle(Socket, Ip, When, Rest).
