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
			
start_link(Name, Config, DB) ->
    gen_server:start_link({local, Name}, ?MODULE, {Config, DB}, []).

%%%==============================================
%%% gen_server callbacks
%%%==============================================

init(Args) ->
    {Config, DB} = Args,
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
	
handle_info({udp, _Socket, IP, _InPortNo, Packet}, State) ->
    spawn(fun () -> handle_packet(State, IP, Packet) end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

			      
terminate(_Reason, Socket) ->
    ok = gen_udp:close(Socket#state.socket),
    {ok, normal}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=======================================================
%%% private function
%%%=======================================================
handle_packet(State, SrcIP, Packet) ->
    DecodePacket = dhcpv4_proto:decode(Packet, SrcIP),
    case DecodePacket of
	{ok, PacketInfo} ->
	    {ok, DstAddress, Reply} =
		dhcpv4_proto:handle_packet(PacketInfo,
					   State#state.config,
					   State#state.db),
	    case Reply of 
		nothing ->
		    ok;
		_ ->
		    {ok, ReplyPacket} =
			dhcpv4_proto:encode(Reply),
		    ok = gen_udp:send(State#state.socket,
				      DstAddress,
				      ?DHCP_DST_PORT, 
				      ReplyPacket)
	    end;
	{nothing} ->
	    ok;
	{error, _} ->
        lager:error("PacketError~n")	    
    end.
    
