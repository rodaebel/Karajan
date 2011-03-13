%% @author Tobias Rodaebel
%% @doc Karajan Event Handler

-module(karajan_handler).

-behaviour(gen_event).

%% API 
-export([start/1, stop/0]).

%% Callbacks of the gen_event behaviour
-export([init/1, code_change/3, handle_event/2, handle_call/2, handle_info/2,
         terminate/2]).

-define(WEBSOCKET_SERVER, {websocket_server, websocket@localhost}).

%% @doc Starts the handler.
%% @spec start(Options) -> already_started | ok
start(Options) ->
    case lists:member(?MODULE, gen_event:which_handlers(karajan_event)) of
        true  ->
            already_started;
        false ->
            gen_event:add_sup_handler(karajan_event, ?MODULE, Options)
    end.

%% @doc Stops the handler.
%% @spec stop() -> ok
stop() ->
    gen_event:delete_handler(karajan_event, ?MODULE, []).

%% @doc Initializes the handler.
%% @spec init(Args) -> {ok, initialized}
init(_Args) ->
    {ok, initialized}.

%% @private
%% @doc Handles events.
%% @spec handle_event(Event, State) -> {ok, State}
handle_event({{_When,[_,"start_stop"],[1.0]},_Socket,_Ip}, State)->
    gen_server:cast(karajan_clock, start),
    {ok, State};
handle_event({{_When,[_,"start_stop"],[0.0]},_Socket,_Ip}, State)->
    gen_server:cast(karajan_clock, stop),
    {ok, State};
handle_event({{_When,["accxyz"],XYZ},_Socket,_Ip}, State)->
    gen_server:cast(?WEBSOCKET_SERVER, {accelerometer, XYZ}),
    {ok, State};
handle_event({{_When,[_,"scale"],S},_Socket,_Ip}, State)->
    gen_server:cast(?WEBSOCKET_SERVER, {scale, S}),
    {ok, State};
handle_event({{_When,[_,"fader1"],S},_Socket,_Ip}, State)->
    gen_server:cast(?WEBSOCKET_SERVER, {scale, S}),
    {ok, State};
handle_event(Event, State)->
    error_logger:info_msg("~p ~p~n", [self(), Event]),
    {ok, State}.

%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, State) -> {ok, {error, bad_query}, Events}
handle_call(_Query, State) ->
    {ok, {error, bad_request}, State}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State}
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc Performs cleanup on termination.
%% @spec terminate(Reason, State) -> ok
terminate(_Reason, _Events) ->
    ok.

%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, Library, Extra) -> {ok, Library}
code_change(_OldVsn, Library, _Extra) ->
    {ok, Library}.
