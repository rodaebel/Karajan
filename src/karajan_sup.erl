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

    %% The Karajan server.
    Server = {
        karajan_server, {karajan_server, start_link, []},
	    permanent, 2000, worker, [karajan_server]},

    %% A ZeroConf server enables Karajan for automatic discovery of devices.
    ZeroConf = {
        karajan_zeroconf, {karajan_zeroconf, start_link, []},
	    permanent, 2000, worker, [karajan_zeroconf]},

    {ok, {{one_for_one, 3, 10}, [Server, ZeroConf]}}.
