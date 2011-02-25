%% @author Tobias Rodaebel
%% @doc Karajan Supervisor

-module(karajan_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% @doc Starts the supervisor.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initializes the supervisor.
%% @spec init(Args) -> {ok, {SupFlags, ChildSpecs}} | ignore | {error, Reason}
init([]) ->

    %% The server process listens for incoming UDP packets. It decodes
    %% received data and fires events containg the OSC message.
    Server = {
        karajan_server, {karajan_server, start_link, []},
	    permanent, 2000, worker, [karajan_server]},
    %% This is the event manager process.
    Event = {
        karajan_event, {gen_event, start_link, [{local, karajan_event}]},
        permanent, 2000, worker, dynamic},
    %% Another server process takes over the role of a guard for our event
    %% handler and restarts it in case of an unexpected crash.
    Guard = {
        karajan_guard, {karajan_guard, start_link, [karajan_handler]},
	    permanent, 2000, worker, [karajan_guard]},
    %% This server process provides a simple clock for synchronizing devices.
    Clock = {
        karajan_clock, {karajan_clock, start_link, []},
	    permanent, 2000, worker, [karajan_clock]},

    {ok, {{one_for_one, 3, 10}, [Server, Event, Guard, Clock]}}.
