-module(dhcpv4_server).

-behavior(gen_server).

-include("dhcp.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DHCP_DST_PORT, 68).

-record( state, { socket :: inet:socket(),
		  config :: dhcp:dhcp_config(),
		  db     :: dhcp_lease_db()
		}).

-spec tup2list(tuple()) -> list().    
tup2list(Tuple) ->
    [ element(I, Tuple) ||
	I <- lists:seq(1, tuple_size(Tuple))].
			
start_link(Name, Config, DB) ->
    io:format("start_link, dhcpv4_server: ~s~n",
	      [Name]),
    gen_server:start_link({local, Name}, ?MODULE, {Config, DB}, []).

%%%==============================================
%%% gen_server callbacks
%%%==============================================

init(Args) ->
    io:format("Args:  ~w~n", [Args]),
    {Config, DB} = Args,
    %% {ok, Config} = dhcpv4_config:load(Args#dhcp_args.config),
    {ok, Socket} = gen_udp:open( 67, [binary,
				      inet, 
				      {broadcast, true},
				      {reuseaddr, true}
				 ]),    
    {ok, #state{ socket = Socket, config = Config, db = DB}}.
        
handle_call(_Request, _From, State) ->
    {reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
	
handle_info({udp, _Socket, _IP, _InPortNo, Packet}, State) ->
    spawn(fun () -> handle_packet(State, Packet) end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

			      
terminate(_Reason, Socket) ->
    io:format("terminate~n"),
    ok = gen_udp:close(Socket#state.socket),
    {ok, normal}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=======================================================
%%% private function
%%%=======================================================
handle_packet(State, Packet) ->
    DecodePacket = dhcpv4_proto:decode(Packet),
    case DecodePacket of
	{ok, PacketInfo} ->
	    {ok, DstAddress, Reply} =
		dhcpv4_proto:handle_packet(PacketInfo,
					   State#state.config,
					   State#state.db),
	    {ok, ReplyPacket} = dhcpv4_proto:encode(Reply),
	    ok = gen_udp:send(State#state.socket,
			      DstAddress,
			      ?DHCP_DST_PORT, 
			      ReplyPacket);
	{nothing} ->
	    ok;
	{error, _} ->
	    io:format("PacketError~n")	    
    end.

    
