%% @author Tobias Rodaebel
%% @doc Karajan ZeroConf Server

-module(karajan_zeroconf).

-behaviour(gen_server).

%% Server API
-export([start_link/0]).

%% Callbacks of the gen_server behaviour
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-include_lib("kernel/src/inet_dns.hrl").

-record(state, {port=null, socket=null, clients=null}).

-record(client, {key, domain, host, port, modified}).

-define(SERVER, ?MODULE). 

-define(OSC_DOMAIN, "_osc._udp.local").

%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
%% @doc Gets a timestamp in milliseconds from the epoch.
%% @spec get_timestamp() -> integer()
get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

%% @private
%% @doc Initializes the server.
%% @spec init(Args) -> {ok, State} | {stop, Reason}
init([]) ->
    Address = {224,0,0,251},
    Port = 5353,
    Options = [binary, {reuseaddr,true}, {ip,Address}, {multicast_ttl,4},
               {multicast_loop,false}],
    case gen_udp:open(Port, Options) of
        {ok, Socket} ->
            inet:setopts(Socket, [{add_membership,{Address,{0,0,0,0}}}]),
            Clients = dict:new(),
            State = #state{port=Port, socket=Socket, clients=Clients},
            {ok, State};
        {error, Reason} ->
            error_logger:error_report({?MODULE, udp_open, Reason}),
            {stop, {?MODULE, udp_open, Reason}}
    end.

%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                            {noreply, State}
handle_call(get_clients, _From, State) ->
    {reply, dict:fetch_keys(State#state.clients), State};
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
handle_info({udp, _Socket, _Ip, _Port, Packet},
            #state{port=Port,socket=Socket,clients=Clients} = State) ->
    case process_dnsrec(inet_dns:decode(Packet)) of
        [] ->
            {noreply, State};
        [Client] ->
            Dict = dict:store(Client#client.key, Client, Clients),
            error_logger:info_msg("~p ~p~n", [self(), Client]),
            {noreply, #state{port=Port,socket=Socket,clients=Dict}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc Processes DNS record.
%% @spec process_dnsrec(Record) -> any()
process_dnsrec({ok, #dns_rec{anlist=[]}}) ->
    [];
process_dnsrec({ok, #dns_rec{anlist=Records}}) ->
    process_records(Records, #client{}).

%% @private
%% @doc Process DNS resource records.
%% @spec process_records(Records::list(), Acc::list()) -> ok
process_records([], #client{key=Key,domain=Domain} = Client) ->
    case {Key, Domain} of
        {undefined, _} ->
            [];
        {_, ?OSC_DOMAIN} ->
            [Client]
    end;
process_records([#dns_rr{domain=Key,type=srv,data=Data}|Rest],
                #client{domain=Domain} = _Client) ->
    {_,_,Port,Host} = Data,
    T = get_timestamp(),
    Cli = #client{key=Key,domain=Domain,host=Host,port=Port,modified=T},
    process_records(Rest, Cli);
process_records([#dns_rr{domain=?OSC_DOMAIN,type=ptr}|Rest],
                #client{key=Key,host=Host,port=Port} = _Client) ->
    T = get_timestamp(),
    Cli = #client{key=Key,host=Host,domain=?OSC_DOMAIN,port=Port,modified=T},
    process_records(Rest, Cli);
process_records([_|Rest], Client) ->
    process_records(Rest, Client).

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
