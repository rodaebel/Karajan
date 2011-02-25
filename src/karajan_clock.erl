%% @author Tobias Rodaebel
%% @doc Karajan Simple Clock Server

-module(karajan_clock).

-behaviour(gen_server).

%% Server API
-export([start_link/0, loop/1]).

%% Callbacks of the gen_server behaviour
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-include("karajan.hrl").

-define(SERVER, ?MODULE). 

-define(BROADCAST_ADDR, {255, 255, 255, 255}).

%% @doc Starts the clock server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
%% @doc Initializes the clock server.
%% @spec init(Args) -> {ok, State} | {stop, Reason}
init([]) ->
    {ok, Port} = application:get_env(outgoing_port),
    Options = [binary, {broadcast, true}],
    case gen_udp:open(0, Options) of
    	{ok, Socket} ->
            State = #clock_state{port=Port, socket=Socket},
	        {ok, State};
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
handle_cast(start, State = #clock_state{run=Run,port=Port,socket=Socket}) ->
    case Run of
    false ->
        Time = calendar:universal_time(),
        process_flag(trap_exit, true),
        Pid = spawn_link(?MODULE, loop, [{Socket, Port, Time}]),
        Msg = {message, "/1/start_stop", [1.0]},
        gen_udp:send(Socket, ?BROADCAST_ADDR, Port, osc_lib:encode(Msg)),
        {noreply, #clock_state{run=true,proc=Pid,port=Port,socket=Socket}};
    true ->
        {noreply, State}
    end;
handle_cast(stop, #clock_state{proc=Pid,port=Port,socket=Socket}) ->
    exit(Pid, stop),
    Msg = {message, "/1/start_stop", [0.0]},
    gen_udp:send(Socket, ?BROADCAST_ADDR, Port, osc_lib:encode(Msg)),
    {noreply, #clock_state{run=false,port=Port,socket=Socket}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State}
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc Performs cleanup on termination.
%% @spec terminate(Reason, State) -> ok
terminate(_Reason, State) ->
    gen_udp:close(State#clock_state.socket),
    ok.

%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, Library, Extra) -> {ok, Library}
code_change(_OldVsn, Library, _Extra) ->
    {ok, Library}.

%% @private
%% @doc The clock loop.
%% @spec loop({Socket, Port, Time}) -> void()
loop({Socket, Port, Time}) ->
    {_, {H, M, S}} = calendar:time_difference(Time, calendar:universal_time()),
    TimeString = lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B", [H,M,S])),
    Msg = {message, "/1/clock", [TimeString]},
    gen_udp:send(Socket, ?BROADCAST_ADDR, Port, osc_lib:encode(Msg)), 
    ok = timer:sleep(100),
    loop({Socket, Port, Time}).
