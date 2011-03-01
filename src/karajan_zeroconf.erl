%% @author Tobias Rodaebel
%% @doc Karajan ZeroConf Server

-module(karajan_zeroconf).

-behaviour(gen_server).

%% Server API
-export([start_link/0, send/1]).

%% Callbacks of the gen_server behaviour
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-include("karajan.hrl").

-include("zeroconf.hrl").

-define(SERVER, ?MODULE). 

%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
            inet:setopts(Socket,[{add_membership,{Address,{0,0,0,0}}}]),
            State = #zeroconf_state{port=Port, socket=Socket},
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
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State}
handle_info({udp, _Socket, Ip, Port, Packet}, State) ->
    error_logger:info_msg("~p From: ~p~nPort: ~p~nData: ~p~n",
                          [self(), Ip, Port, inet_dns:decode(Packet)]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc Performs cleanup on termination.
%% @spec terminate(Reason, State) -> ok
terminate(_Reason, State) ->
    gen_udp:close(State#zeroconf_state.socket),
    ok.

%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, Library, Extra) -> {ok, Library}
code_change(_OldVsn, Library, _Extra) ->
    {ok, Library}.


%% @doc Sends ZeroConf packets.
%%      Taken from Jarrod Roberson's implementation. See his blog for details:
%%      http://www.vertigrated.com/blog/2009/11/bonjour-zeroconf-in-erlang
%% @spec send(Domain) -> any()
send(Domain) ->
    Options = [binary, {reuseaddr,true}, {ip,{224,0,0,251}}, {multicast_ttl,4},
               {multicast_loop,false}, {broadcast,true}],
    {ok, Socket} = gen_udp:open(5353, Options),
    Packet = #dns_rec{header=#dns_header{},
                      qdlist=[#dns_query{domain=Domain,type=ptr,class=in}]},
    gen_udp:send(Socket, {224,0,0,251}, 5353, inet_dns:encode(Packet)),
    gen_udp:close(Socket).
