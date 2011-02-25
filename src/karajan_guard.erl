%% @author Tobias Rodaebel
%% @doc Karajan Event Handler Guard
%%
%% This guard starts our event handler and helps to restart it in case of an
%% unexpected crash.

-module(karajan_guard).

-behavior(gen_server).

%% Guard API
-export([start_link/1]).

%% Callbacks of the gen_server behaviour
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

%% @doc Starts the guard.
%% @spec start_link(Handler) -> {ok, Pid} | ignore | {error, Reason}
start_link(Handler) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Handler, []).

%% @doc Initializes the guard and starts the handler.
%% @spec init(Handler) -> {ok, Handler} | {stop, {already_started, Handler}} |
%%                        {stop, Reason}
init(Handler) ->
    case catch Handler:start([]) of
        ok ->
            {ok, Handler};
        already_started ->
            {stop, {already_started, Handler}};
        Error ->
            {stop, Error}
    end.

%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, From, State) -> {noreply, State}
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handles cast messages.
%% @spec handle_cast(Msg, State) -> {noreply, State}
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State}
handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    io:format("~p ~w detected handler ~p shutdown (Reason: ~p)~n",
              [self(), ?MODULE, Handler, Reason]),
    {stop, {handler_died, Handler, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

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
