%% @author Tobias Rodaebel
%% @doc Karajan Entry Point

-module(karajan).

-export([start/0, stop/0]).

%% @doc Starts the server.
%% @spec start() -> ok | {error, Reason}
start() ->
    application:start(karajan).

%% @doc Stops the server.
%% @spec stop() -> ok
stop() ->
    application:stop(karajan).
