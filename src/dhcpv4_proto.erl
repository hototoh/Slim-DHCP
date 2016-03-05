-module(dhcpv4_proto).

-export([encode/1, decode/1, handle_packet/3]).

-include("dhcp.hrl").

-record(handle_options, {
	  ip_addr = {0,0,0,0} :: dhcp:ip(),
	  client_type = 0 :: dhcp:client_id(),
	  client_id = << >> :: binary(),
	  mac = << >> :: binary(),
	  message_type :: dhcp:message_type(),
	  options = [] :: list()
	 }).

-spec to_lbinary(uint32() | int32()) -> binary().
to_lbinary(X) ->
    <<X:32>>.

-spec to_sbinary(uint16() | int16()) -> binary() .
to_sbinary(X) ->
    << X:16 >>.

-spec tuple_to_binary(tuple()) -> binary().
tuple_to_binary(T) ->
    list_to_binary(tuple_to_list(T)).

-spec binary_to_tuple(binary()) -> ip().
binary_to_tuple(<<A:8, B:8, C:8, D:8>>) ->
    {A,B,C,D};
binary_to_tuple(<<A:8,B:8,C:8,D:8,E:8,F:8>>) ->
    {A,B,C,D,E,F}.

-spec tuple_to_binary_pair(ip_pair()) -> binary().
tuple_to_binary_pair({A1, A2}) ->
    B1 = tuple_to_binary(A1),
    B2 = tuple_to_binary(A2),
    << B1, B2 >>.       

-spec dhcp_message_decode(byte()) -> message_type() | error.
dhcp_message_decode(Id) ->
    dhcp_message_type_map(Id).
-spec dhcp_message_encode(message_type()) -> byte() | error.
dhcp_message_encode(Type) ->
    dhcp_message_type_map(Type).

-define(DHCP_MESSAGE_TYPE_MAP(Id, Type),
        dhcp_message_type_map(Id) -> Type;
        dhcp_message_type_map(Type) -> Id).
?DHCP_MESSAGE_TYPE_MAP(1, dhcpdiscover);
?DHCP_MESSAGE_TYPE_MAP(2, dhcpoffer);
?DHCP_MESSAGE_TYPE_MAP(3, dhcprequest);
?DHCP_MESSAGE_TYPE_MAP(4, dhcpdecline);
?DHCP_MESSAGE_TYPE_MAP(5, dhcpack);
?DHCP_MESSAGE_TYPE_MAP(6, dhcpnak);
?DHCP_MESSAGE_TYPE_MAP(7, dhcprelease);
?DHCP_MESSAGE_TYPE_MAP(8, dhcpinform);
dhcp_message_type_map(_) -> error.

-spec dhcp_message_type(list()) -> boolean() | tuple().
dhcp_message_type(Options) ->
    lists:keyfind(dhcp_message_type, 1, Options).

-spec dhcp_option_decode(byte()) -> dhcp_option() | error.
dhcp_option_decode(Id) ->
    dhcp_option_map(option, Id).
-spec dhcp_option_encode(dhcp_option()) -> byte() | error.
dhcp_option_encode(Option) ->
    dhcp_option_map(id, Option).
-spec dhcp_option_type(dhcp_option()) -> option_type() | error.
dhcp_option_type(Option) ->
    dhcp_option_map(type, Option).

-define(DHCP_OPTION_MAP(Id, Option, Type),
        dhcp_option_map(option, Id) -> Option;
        dhcp_option_map(id, Option) -> Id;
        dhcp_option_map(type, Option) -> Type).
?DHCP_OPTION_MAP(0, dhcp_pad, empty);
?DHCP_OPTION_MAP(1, dhcp_subnet, ipaddr);
?DHCP_OPTION_MAP(2, dhcp_time_offset, int32_t);
?DHCP_OPTION_MAP(3, dhcp_router, ipaddr_list);
?DHCP_OPTION_MAP(4, dhcp_time_server, ipaddr_list);
?DHCP_OPTION_MAP(5, dhcp_name_server, ipaddr_list);
?DHCP_OPTION_MAP(6, dhcp_domain_name_server, ipaddr_list);
?DHCP_OPTION_MAP(7, dhcp_log_server, ipaddr_list);
?DHCP_OPTION_MAP(8, dhcp_cookie_server, ipaddr_list);
?DHCP_OPTION_MAP(9, dhcp_lpr_server, ipaddr_list);
?DHCP_OPTION_MAP(10, dhcp_impress_server, ipaddr_list);
?DHCP_OPTION_MAP(11, dhcp_resource_location_server, ipaddr_list);
?DHCP_OPTION_MAP(12, dhcp_host_name, string);
?DHCP_OPTION_MAP(13, dhcp_boot_file_size, uint16_t);
?DHCP_OPTION_MAP(14, dhcp_merit_dump_fie, string);
?DHCP_OPTION_MAP(15, dhcp_domain_name, string);
?DHCP_OPTION_MAP(16, dhcp_swap_server, ipaddr);
?DHCP_OPTION_MAP(17, dhcp_root_path, string);
?DHCP_OPTION_MAP(18, dhcp_extensions_path, string);
?DHCP_OPTION_MAP(19, dhcp_ip_forwarding, byte);
?DHCP_OPTION_MAP(20, dhcp_non_local_source_routing, byte);
?DHCP_OPTION_MAP(21, dhcp_policy_filter, ipaddr_pair_list);
?DHCP_OPTION_MAP(22, dhcp_max_datagram_size, uint16_t);
?DHCP_OPTION_MAP(23, dhcp_ip_ttl, byte);
?DHCP_OPTION_MAP(24, dhcp_mtu_aging_time, uint32_t);
?DHCP_OPTION_MAP(25, dhcp_mtu_plateau, uint16_t_list);
?DHCP_OPTION_MAP(26, dhcp_interface_mtu, uint16_t);
?DHCP_OPTION_MAP(27, dhcp_subnets_local, byte);
?DHCP_OPTION_MAP(28, dhcp_broadcast_address, ipaddr);
?DHCP_OPTION_MAP(29, dhcp_mask_discovery, byte);
?DHCP_OPTION_MAP(30, dhcp_mask_supplier, byte);
?DHCP_OPTION_MAP(31, dhcp_router_discovery, byte);
?DHCP_OPTION_MAP(32, dhcp_router_solicitation_address, ipaddr);
?DHCP_OPTION_MAP(33, dhcp_static_route, ipaddr_pair_list);
?DHCP_OPTION_MAP(34, dhcp_trailer_encap, byte);
?DHCP_OPTION_MAP(35, dhcp_arp_cach_timeout, uint32_t);
?DHCP_OPTION_MAP(36, dhcp_ether_encap, byte);
?DHCP_OPTION_MAP(37, dhcp_tcp_ttl, byte);
?DHCP_OPTION_MAP(38, dhcp_tcp_keepalive_interval, uint32_t);
?DHCP_OPTION_MAP(39, dhcp_tcp_keepalive_garbage, byte);
?DHCP_OPTION_MAP(40, dhcp_net_info_service_domain, string);
?DHCP_OPTION_MAP(41, dhcp_net_info_server, ipaddr_list);
?DHCP_OPTION_MAP(42, dhcp_net_time_proto_server, ipaddr_list);
?DHCP_OPTION_MAP(43, dhcp_vendor_info, vendor_info);
?DHCP_OPTION_MAP(44, dhcp_netbios_name_server, ipaddr_list);
?DHCP_OPTION_MAP(45, dhcp_netbios_datagram_dist_server, ipaddr_list);
?DHCP_OPTION_MAP(46, dhcp_netbios_node_type, byte);
?DHCP_OPTION_MAP(47, dhcp_netbios_scope, string);
?DHCP_OPTION_MAP(48, dhcp_xwindow_font_server, ipaddr_list);
?DHCP_OPTION_MAP(49, dhcp_xwindow_display_manager, ipaddr_list);
?DHCP_OPTION_MAP(50, dhcp_req_ip_addr, ipaddr);
?DHCP_OPTION_MAP(51, dhcp_ip_addr_lease_time, uint32_t);
?DHCP_OPTION_MAP(52, dhcp_overload, byte);
?DHCP_OPTION_MAP(53, dhcp_message_type, byte);
?DHCP_OPTION_MAP(54, dhcp_server_id, ipaddr);
?DHCP_OPTION_MAP(55, dhcp_param_req_list, string);
?DHCP_OPTION_MAP(56, dhcp_message, string);
?DHCP_OPTION_MAP(57, dhcp_max_dhcp_message_size, uint16_t);
?DHCP_OPTION_MAP(58, dhcp_renewal_time, uint32_t);
?DHCP_OPTION_MAP(59, dhcp_rebinding_time, uint32_t);
?DHCP_OPTION_MAP(60, dhcp_vendor_class_id, string);
?DHCP_OPTION_MAP(61, dhcp_client_id, client_id);
?DHCP_OPTION_MAP(66, dhcp_tftp_server, string);
?DHCP_OPTION_MAP(67, dhcp_bootfile, string);
?DHCP_OPTION_MAP(255, dhcp_end, empty);
dhcp_option_map(_, __) -> 
    error.

-spec dhcp_flags_decode(uint16()) -> dhcp_flags().
dhcp_flags_decode(X) ->
    <<Y:1, _:15>> = <<X:16>>,
    case Y of
	0 ->
	    unicast;
	1 ->
	    broadcast		
    end.

-spec dhcp_flags_encode(dhcp_flags()) -> binary().
dhcp_flags_encode(broadcast) ->
    <<1:1, 0:15>>;
dhcp_flags_encode(unicast) ->
    <<0:1, 0:15>>.
    
%%%==============================================================
%%% encode function
%%%==============================================================
encode_option(OptionName, Body) ->
    OptionType = dhcp_option_type(OptionName),
    OptionCode = dhcp_option_encode(OptionName),
    case OptionType of 
	empty->
	    << OptionCode:8 >>;
	ipaddr -> 
	    BinBody = tuple_to_binary(Body),
	    << OptionCode:8 , 4:8, BinBody/binary >>;
	ipaddr_list ->
	    BinBody = list_to_binary(
			lists:map(fun(X) -> tuple_to_binary(X) end, Body)),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	ipaddr_pair -> 	    
	    BinBody = tuple_to_binary_pair(Body),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	ipaddr_pair_list ->
	    BinBody = list_to_binary(
			lists:map(fun(X) -> tuple_to_binary_pair(X) end, Body)),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	byte  ->
	    << OptionCode:8, 1:8, Body:8 >>;
	string ->
	    BinBody = << Body/binary >>, 
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	int16_t -> 
	    BinBody = to_sbinary(Body),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	int32_t ->
	    BinBody = to_lbinary(Body),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	int16_t_list -> 
	    BinBody = list_to_binary(
			lists:map(fun(X) -> to_sbinary(X) end, Body)),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	int32_t_list -> 
	    BinBody = list_to_binary(
			lists:map(fun(X) -> to_lbinary(X) end, Body)),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	uint16_t -> 
	    BinBody = to_sbinary(Body),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	uint32_t ->
	    BinBody = to_lbinary(Body),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	uint16_t_list ->
	    BinBody = list_to_binary(
			lists:map(fun(X) -> to_sbinary(X) end, Body)),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	uint32_t_list -> 
	    BinBody = list_to_binary(
			lists:map(fun(X) -> to_lbinary(X) end, Body)),
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	client_id  ->
	    {ClientIdType, ClientId} = Body,
	    BinClientId = tuple_to_binary(ClientId),
	    BinBody = << ClientIdType:8, BinClientId >>,
	    BinSize = size(BinBody),
	    << OptionCode:8, BinSize:8, BinBody/binary >>;
	vendor_info ->
	    not_imple
    end.
	    
	
-spec encode_options(list()) -> binary().
encode_options(Options) ->
    BinOptions = encode_options(Options, <<>>),
    PaddingBins = case ((size(BinOptions)+1) rem 4) of
		      3 -> << 0:40 >>;
		      2 -> << 0:48 >>;
		      1 -> << 0:56 >>;
		      0 -> << >>
		  end,    
    << BinOptions/binary, 255:8, PaddingBins/binary >>.

-spec encode_options(list(), binary()) -> binary().
encode_options([], Bins) ->
    Bins;
encode_options([Option|Next], Bins) ->
    {OptionName, Body} = Option,    
    BinOption    = encode_option(OptionName, Body),
    encode_options(Next, << Bins/binary, BinOption/binary >>).

-spec encode(dhcp:dhcp_packet()) -> {ok, binary()}.
encode(PacketInfo) ->
    Op            = PacketInfo#dhcp_packet.op,
    Htype         = PacketInfo#dhcp_packet.htype,
    Hlen          = PacketInfo#dhcp_packet.hlen,
    Hops          = PacketInfo#dhcp_packet.hops ,
    Xid           = PacketInfo#dhcp_packet.xid,
    Secs          = PacketInfo#dhcp_packet.secs,
    Flags         = dhcp_flags_encode(PacketInfo#dhcp_packet.flags) ,
    CIAddr        = tuple_to_binary(PacketInfo#dhcp_packet.ciaddr) ,
    YIAddr        = tuple_to_binary(PacketInfo#dhcp_packet.yiaddr) ,
    SIAddr        = tuple_to_binary(PacketInfo#dhcp_packet.siaddr) ,
    GIAddr        = tuple_to_binary(PacketInfo#dhcp_packet.giaddr) ,
    CHAddr        = tuple_to_binary(PacketInfo#dhcp_packet.chaddr) ,
    SName         = PacketInfo#dhcp_packet.sname ,
    File          = PacketInfo#dhcp_packet.file ,
    PackedOptions = encode_options(PacketInfo#dhcp_packet.options),
    Packet = << Op:8,
		Htype:8,
		Hlen:8,
		Hops:8,
		Xid:32,
		Secs:16,
		Flags:2/binary,
		CIAddr:4/binary,
		YIAddr:4/binary,
		SIAddr:4/binary,
		GIAddr:4/binary,
		CHAddr:6/binary,
		0:80,
		SName:64/binary,
		File:128/binary,
		99:8, 130:8, 83:8, 99:8,
		PackedOptions/binary >>,
    {ok , Packet}.

%%%==============================================================
%%% decode function
%%%==============================================================

-spec decode(binary()) -> {ok, dhcp:dhcp_packet()} |
			  {error, message_type} |
			  {error, unknown}.
decode(<<Op:8,
	 Htype:8,
	 Hlen:8,
	 Hops:8,
	 Xid:32,
	 Secs:16,
	 Flags:16,
	 CIAddr:4/binary,
	 YIAddr:4/binary,
	 SIAddr:4/binary,
	 GIAddr:4/binary,
	 CHAddr:6/binary,	   
	 0:80,
	 SName:64/binary,
	 File:128/binary,
	 99:8, 130:8, 83:8, 99:8,
	 PackedOptions/binary>>
	) when Op == 1 ->
    case decode_options(PackedOptions) of 
	{ok, MessageType, Options} ->
	    DHCPPacket =  #dhcp_packet
		{
		  message_type = MessageType,
		  op      = Op,
		  htype   = Htype,
		  hlen    = Hlen,
		  hops    = Hops,
		  xid     = Xid, 
		  secs    = Secs, 
		  flags   = dhcp_flags_decode(Flags),
		  ciaddr  = binary_to_tuple(CIAddr),
		  yiaddr  = binary_to_tuple(YIAddr),
		  siaddr  = binary_to_tuple(SIAddr),
		  giaddr  = binary_to_tuple(GIAddr),
		  chaddr  = binary_to_tuple(CHAddr),
		  sname   = SName,
		  file    = File,
		  options = Options
		},
	    {ok, DHCPPacket};
	{ok, _Options} ->
	    lager:error("Unknown DHCP message type.~n"),
	    {error, message_type};
	{error} ->
	    lager:error("Invalid options~n."),
	    {error, unknown};
	Other ->
	    lager:error("Invalid options : ~w~n.", [Other])
    end;
decode(_) ->
    {error, unknown}.

-spec decode_options(binary()) -> {ok, list()} | error.
decode_options(Options) ->
    decode_options(Options, []).

-spec decode_options(binary(), list()) -> {ok, list()} |
					  error.
decode_options(<<>>, Options) ->
    {ok, Options};
decode_options(<<0:8, Next/binary>>, Options) ->    
    decode_options(Next, Options);
decode_options(<<255:8,_/binary>>, Options) ->    
    TypeTuple = dhcp_message_type(Options),
    case is_tuple(TypeTuple) of
	true ->
	    {dhcp_message_type, <<MessageType>>} = TypeTuple,
	    {ok,
	     dhcp_message_decode(MessageType),
	     lists:reverse(Options) };
	false ->
	    {ok, Options}
    end;
decode_options(<<Code:8, Len:8, Body/binary>>, Options) ->    
    << OptionBody:Len/binary, Next/binary >> = Body,
    case dhcp_option_decode(Code) of
	error ->
	    error;
	OptionName ->
	    decode_options(Next,
			   [ {OptionName, OptionBody} |
			     Options])
    end;
decode_options(_, _)->
    error.

%%%=======================================================
%%% handle function
%%%=======================================================
-spec filled_packet_options(dhcp:dhcp_packet(),
			    dhcp:dhcp_config())
			   -> dhcp:dhcp_packet().
filled_packet_options(PacketInfo, Config) ->
    RequestedOptions = PacketInfo#dhcp_packet.options,
    GetAvailableValue =
	fun(Items) ->
		{Key, Value} = Items,
		case Value of
		    undefine ->
			Option = maps:get(Key, Config#dhcp_config.options, error),
			case Option of
			    error  ->
				false;
			    Option -> {true, {Key, Option}}
			end;
		    _ ->
			{true, {Key, Value}}
		end
	end,

    ReplyOptions = lists:filtermap(GetAvailableValue, RequestedOptions),
    PacketInfo#dhcp_packet {
      options = ReplyOptions
     }.

-spec filled_client_ip_addr(dhcp:handle_options(),
			    dhcp:dhcp_packet()) ->
				   dhcp:handle_options().
filled_client_ip_addr(ClientOptions, PacketInfo) ->
    case PacketInfo#dhcp_packet.ciaddr of
	{0,0,0,0} ->
	    ClientOptions;
	_ ->
	    ClientOptions#handle_options {
	      ip_addr = PacketInfo#dhcp_packet.ciaddr
	     }
    end.
-spec filled_client_mac_addr(dhcp:handle_options(),
			     dhcp:dhcp_packet()) ->
				    dhcp:handle_options().
filled_client_mac_addr(ClientOptions, PacketInfo) ->
    MACAddr = PacketInfo#dhcp_packet.chaddr,
    case ClientOptions#handle_options.client_id of
	<< >> ->
	    ClientOptions#handle_options {
	      client_type = 1,
	      client_id = tuple_to_binary(MACAddr),
	      mac = MACAddr
	     };
	_  -> 
	    ClientOptions#handle_options {
	      mac = MACAddr
	     }
    end.

-spec filled_handle_options(dhcp:handle_options(),
			    dhcp:dhcp_packet()) ->
				   dhcp:handle_options().
filled_handle_options(ClientOptions, PacketInfo) ->
    filled_client_mac_addr(
      filled_client_ip_addr(ClientOptions, PacketInfo)
      , PacketInfo).

-spec parse_options(list()) -> dhcp:handle_options().
parse_options(Options) ->
    parse_options(Options, #handle_options{}).    

-spec parse_options(list(), dhcp:handle_options()) -> dhcp:handle_options().
parse_options([], ClientOptions) -> 
    ClientOptions;
parse_options([{OptionName, OptionBody} | Next], ClientOptions) -> 
    case OptionName of 
	dhcp_req_ip_addr ->
	    parse_options(Next, ClientOptions#handle_options{
				  ip_addr = binary_to_tuple(OptionBody)
				 });
	dhcp_client_id ->
	    << ClientId:8, MACAddr/binary >> = OptionBody,
	    parse_options(Next, ClientOptions#handle_options{
				  client_type = ClientId,
				  client_id   = MACAddr
				 });
	dhcp_param_req_list ->
	    GetAvailableValue = fun(X) -> 
					Option = dhcp_option_decode(X),
					case Option of 
					    error -> false;
					    Option -> {true, Option}
					end
				end,
	    Options = lists:filtermap(GetAvailableValue,
				      binary_to_list(OptionBody)),
	    parse_options(Next, ClientOptions#handle_options{
				  options = Options
				 });
	_ ->
	    parse_options(Next, ClientOptions)	
    end.

-spec handle_discover_packet(dhcp:dhcp_packet(),
			     dhcp:handle_options(),
			     dhcp:dhcp_lease_state())
			    -> {ok, ip(), dhcp:dhcp_packet()}.
handle_discover_packet(PacketInfo, ClientOptions, DB) ->
    %% ClientOptions has RequestedIPaddr option 
    %% checked it
    ReqIPAddr = ClientOptions#handle_options.ip_addr,
    ClientId = ClientOptions#handle_options.client_id,
    MACAddr = ClientOptions#handle_options.mac,
    Entry = case ReqIPAddr of
		{0,0,0,0} -> 
		    lease_ets:request(
		      DB, 
		      {alloc,
		       #dhcp_lease {
			  client_id = ClientId,
			  mac = MACAddr
			 }
		      });
		_ ->
		    lease_ets:request(
		      DB, 
		      {challenge,
		       #dhcp_lease {    
			  ip_addr = ReqIPAddr,
			  client_id = ClientId,
			  mac = MACAddr
			 }
		      })
	    end,
    case Entry of 
	{error, Reason} ->
	    lager:error("Failed to alloc ~w ~w~n", [ ip2str(ReqIPAddr) ,Reason]);
	_ ->
	    ok
    end,
    %% append default options of dhcpoffer
    %% server_identifier and ip address lease time   
    DefaultOptions = [dhcp_ip_addr_lease_time,
		      dhcp_server_id],
    _MustNotOptions = [dhcp_req_ip_addr, dhcp_max_dhcp_message_size,
		       dhcp_param_req_list, dhcp_client_id],
    Options = [{ dhcp_message_type, dhcp_message_encode(dhcpoffer)} |
	       lists:map(fun (X) -> {X, undefine} end,
			 lists:flatten([DefaultOptions,
					ClientOptions#handle_options.options])
			)],
    lager:info("DISCOVER/OFFER: ~s is assigned to ~s",
	       [ip2str(Entry#dhcp_lease.ip_addr), ether2str(MACAddr)]),
    P = PacketInfo#dhcp_packet{
	  message_type = dhcpoffer,
	  op      = 2,
	  secs    = 0,	  
	  yiaddr  = Entry#dhcp_lease.ip_addr,
	  options = Options
	 },
    {ok, {255,255,255,255}, P}.

-spec handle_request_state(dhcp:dhcp_packet(),
			   dhcp:handle_options())
			  -> atom().
handle_request_state(_PacketInfo, ClientOptions) ->
    HandleOptions = ClientOptions#handle_options.options,
    HasKey = fun(X) -> lists:any(fun(Y) ->
					 Y == X
				 end,
				 HandleOptions)
	     end,
		
    %% Broadcast = PacketInfo#dhcp_packet.flags,
    %% TODO : not flag 255 or not
    Broadcast = broadcast,
    ServerId  = HasKey(dhcp_server_id),
    ReqIPAddr = HasKey(dhcp_req_ip_addr),
    case {Broadcast, ServerId, ReqIPAddr} of
	%{unicast, false, false} ->
	%    renewing;
	{broadcast, true, true} ->
	    selecting;
	{broadcast, false, true} ->
	    initreboot;
	{broadcast, false, false} ->
	    rebinding;
	{_, _, _} ->
	    error
    end.
	  

handle_request_packet(PacketInfo, ClientOptions, DB, dhcpnak) ->
    
    ReqIPAddr = ClientOptions#handle_options.ip_addr,
    ClientId = ClientOptions#handle_options.client_id,
    MACAddr = ClientOptions#handle_options.mac,
    case  lease_ets:request(DB, 
			    {release,
			     #dhcp_lease {
				ip_addr   = ReqIPAddr,
				client_id = ClientId, 
				mac = MACAddr
			       }}) of
	{error, _Reason} ->
	    ok;
	_Entry ->
	    ok
    end,    		   
    PacketInfo#dhcp_packet{
       message_type = dhcpnak,
       op      = 2,
       secs    = 0,
       ciaddr  = {0,0,0,0},
       yiaddr  = {0,0,0,0},
       siaddr  = {0,0,0,0},
       options = [ %% These options's second type is binary format.
		   { dhcp_message_type,
		     dhcp_message_encode(dhcpnak)}
		   , {dhcp_server_id, undefine}
		 ]
      }; 
handle_request_packet(PacketInfo, ClientOptions, _DB, _Other) ->
    ReqIPAddr = ClientOptions#handle_options.ip_addr,
    DefaultOptions = [dhcp_ip_addr_lease_time,
		      dhcp_server_id],
    _MustNotOptions = [dhcp_req_ip_addr, dhcp_max_dhcp_message_size,
		       dhcp_param_req_list, dhcp_client_id],
    Options = [{ dhcp_message_type, dhcp_message_encode(dhcpack)} |
	       lists:map(fun (X) -> {X, undefine} end,
			 lists:flatten([DefaultOptions,
					ClientOptions#handle_options.options])
			)],
    PacketInfo#dhcp_packet{
      message_type = dhcpack,
      op      = 2,
      secs    = 0,
      ciaddr  = {0,0,0,0},
      yiaddr  = ReqIPAddr,
      options = Options
     }.
-spec handle_request_packet(dhcp:dhcp_packet(),
			    dhcp:handle_options(),
			    dhcp:dhcp_lease_state())
			   -> {ok, ip(), dhcp:dhcp_packet()}.
handle_request_packet(PacketInfo, ClientOptions, DB) ->
    State = handle_request_state(PacketInfo, ClientOptions),
    ReqIPAddr = ClientOptions#handle_options.ip_addr,
    ClientId = ClientOptions#handle_options.client_id,
    MACAddr = ClientOptions#handle_options.mac,

    ValidReqIP =
	case lease_ets:request(DB, 
			       {update,
				#dhcp_lease {
				   ip_addr = ReqIPAddr,
				   client_id = ClientId,
				   mac = MACAddr,
				   flag = used
				  }
			       }) of
	    {error, _Reason} ->
		false;
	    _Entry ->
		true
	end,
	
    {DstAddress, ReqType} =
	case {State, ValidReqIP} of 
	    {error, _} ->
		lager:info("REQUEST/NAK: refuse of the request: ~s from ~s",
			   [ip2str(ReqIPAddr), ether2str(MACAddr)]),
		{{255,255,255,255}, dhcpnak};
	    {_, false} ->
		lager:info("REQUEST/NAK: refuse the invalid request: ~s from ~s",
			   [ip2str(ReqIPAddr), ether2str(MACAddr)]),
		{{255,255,255,255}, dhcpnak};
	    {renewing, _} ->
		lager:info("REQUEST/ACK: renewing ~s from ~s",
			   [ip2str(ReqIPAddr), ether2str(MACAddr)]),
		{ReqIPAddr, renewing};
	    {selecting, _} ->
		lager:info("REQUEST/ACK: selecting ~s from ~s",
			   [ip2str(ReqIPAddr), ether2str(MACAddr)]),
		{{255,255,255,255}, selecting};
	    {initreboot, _} ->
		lager:info("REQUEST/ACK: initreboot ~s from ~s",
			   [ip2str(ReqIPAddr), ether2str(MACAddr)]),
		{{255,255,255,255}, initreboot};
	    {rebinding, _} ->
		lager:info("REQUEST/ACK: rebinding ~s from ~s",
			   [ip2str(ReqIPAddr), ether2str(MACAddr)]),
		{{255,255,255,255}, {rebinding}}
	end,
    {ok, DstAddress,
     handle_request_packet(PacketInfo, ClientOptions, DB, ReqType)}.
    
-spec handle_inform_packet(dhcp:dhcp_packet(),
			   dhcp:handle_options(),
			   dhcp:dhcp_lease_state())
			    -> {ok, ip(), dhcp:dhcp_packet()}.
handle_inform_packet(PacketInfo, ClientOptions, _DB) ->
    MACAddr = ClientOptions#handle_options.mac,
    DefaultOptions = [dhcp_server_id],
    _MustNotOptions = [dhcp_req_ip_addr, dhcp_max_dhcp_message_size,
		       dhcp_param_req_list, dhcp_client_id, 
		       dhcp_ip_addr_lease_time ],
    Options = [{ dhcp_message_type, dhcp_message_encode(dhcpack)} |
	       lists:map(fun (X) -> {X, undefine} end,
			 lists:flatten([DefaultOptions,
					ClientOptions#handle_options.options])
			)],
    lager:info("INFORM:  from ~s", [ether2str(MACAddr)]),
	       %% [option2str(ClientOptions#handle_options.options), ether2str(MACAddr)]),
    P = PacketInfo#dhcp_packet{
	  message_type = dhcpack,
	  op      = 2,
	  secs    = 0,
	  options = Options
	 },
    {ok, PacketInfo#dhcp_packet.ciaddr, P}.

-spec handle_release_packet(dhcp:dhcp_packet(),
			   dhcp:handle_options(),
			   dhcp:dhcp_lease_state())
			    -> {ok, nothing, nothing}.
handle_release_packet(PacketInfo, ClientOptions, DB) ->
    MACAddr = ClientOptions#handle_options.mac,
    IPAddr = PacketInfo#dhcp_packet.ciaddr,
    ClientId = ClientOptions#handle_options.client_id,
    lager:info("RELEASE: ~s from ~s", [ip2str(IPAddr), ether2str(MACAddr)]),
    case  lease_ets:request(DB, 
			    {release,
			     #dhcp_lease {
				ip_addr   = IPAddr,
				client_id = ClientId, 
				mac = MACAddr
			       }}) of
	{error, Reason} ->
	    lager:error("Failed to release ~s : ~s",
			[ip2str(IPAddr), Reason]);
	_ ->
	    ok
    end,    		   
    {ok, nothing, nothing}.

%% option2str(Options) ->
%%     "Options" .

ip2str(IP) ->
    {A, B, C, D} = IP,
    io_lib:format("~B.~B.~B.~B", [A, B, C, D]).

ether2str(MACAddr) ->
    {A, B, C, D, E, F} = MACAddr,
    io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B",
		  [A, B, C, D, E, F]).

-spec handle_packet(dhcp:dhcp_packet(),
		    dhcp:dhcp_config(),
		    dhcp:dhcp_lease_db())
		   -> dhcp:dhcp_packet().
handle_packet(PacketInfo, Config, DB) ->
    ParsedOptions = parse_options(PacketInfo#dhcp_packet.options),
    HandleOptions = filled_handle_options(ParsedOptions, PacketInfo),
    MACAddr = HandleOptions#handle_options.mac,
    {ok, DstAddress, ReplyPacket} = 
	case PacketInfo#dhcp_packet.message_type of 
	    dhcpdecline ->
		%% don't free tmp entry
		IPAddr = HandleOptions#handle_options.ip_addr,
		lager:info("DECLINE: ~s from ~s",
			   [ip2str(IPAddr), ether2str(MACAddr)]),
		{ok, nothing, nothing};
	    dhcprelease ->
		%% free used entry
		handle_release_packet(PacketInfo, HandleOptions, DB);
	    dhcpdiscover -> 
		handle_discover_packet(PacketInfo, HandleOptions, DB);
	    dhcprequest ->
		handle_request_packet(PacketInfo, HandleOptions, DB);
	    dhcpinform ->	   
		handle_inform_packet(PacketInfo, HandleOptions, DB)
	end,
    case ReplyPacket of 
	nothing ->
	    {ok, nothing, nothing};
	_ ->
	    {ok, DstAddress, filled_packet_options(ReplyPacket, Config)}
    end.
