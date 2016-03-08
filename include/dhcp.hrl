-type message_type() ::
	dhcpdiscover | dhcpoffer | dhcprequest |
	dhcpdecline | dhcpack | dhcpnack | dhcprelease |
	dhcpinform.

-type dhcp_flags() :: broadcast | unicast.

-type uint32() :: 0..4294967295.
-type int32()  :: -2147483648..2147483647.
-type uint16() :: 0..65535.
-type int16() :: -32768..32767.

-type ip() ::
	{ byte(), byte(), byte(), byte()}.
-type ip_pair() ::
	{ip(), ip()}.
-type ether() ::
	{ byte(), byte(), byte(), byte(), byte(), byte()}.
-type client_id() ::  byte().
	
-type option_type() :: ipaddr | ipaddr_list |
		       ipaddr_pair | ipaddrs_pair_list |
		       byte | string |
		       int16_t | int16_t_list | 
		       uint16_t | uint16_t_list | 
		       uint32_t | uint32_t_list | 
		       int32_t | int32_t_list | 
		       empty | client_id | vendor_info.
		       
-type dhcp_option() :: dhcp_pad |
		       dhcp_subnet |
		       dhcp_time_offset |
		       dhcp_router |
		       dhcp_time_server |
		       dhcp_name_server |
		       dhcp_domain_name_server |
		       dhcp_log_server |
		       dhcp_cookie_server |
		       dhcp_lpr_server |
		       dhcp_impress_server |
		       dhcp_resource_location_server |
		       dhcp_host_name |
		       dhcp_boot_file_size |
		       dhcp_merit_dump_fie |
		       dhcp_domain_name |
		       dhcp_swap_server |
		       dhcp_root_path |
		       dhcp_extensions_path |
		       dhcp_ip_forwarding |
		       dhcp_non_local_source_routing |
		       dhcp_policy_filter |
		       dhcp_max_datagram_size |
		       dhcp_ip_ttl |
		       dhcp_mtu_aging_time |
		       dhcp_mtu_plateau |
		       dhcp_interface_mtu |
		       dhcp_subnets_local |
		       dhcp_broadcast_address |
		       dhcp_mask_discovery |
		       dhcp_mask_supplier |
		       dhcp_router_discovery |
		       dhcp_router_solicitation_address |
		       dhcp_static_route |
		       dhcp_trailer_encap |
		       dhcp_arp_cach_timeout |
		       dhcp_ether_encap |
		       dhcp_tcp_ttl |
		       dhcp_tcp_keepalive_interval |
		       dhcp_tcp_keepalive_garbage |
		       dhcp_net_info_service_domain |
		       dhcp_net_info_server |
		       dhcp_net_time_proto_server |
		       dhcp_vendor_info |
		       dhcp_netbios_name_server |
		       dhcp_netbios_datagram_dist_server |
		       dhcp_netbios_node_type |
		       dhcp_netbios_scope |
		       dhcp_xwindow_font_server |
		       dhcp_xwindow_display_manager |
		       dhcp_req_ip_addr |
		       dhcp_ip_addr_lease_time |
		       dhcp_overload |
		       dhcp_message_type |
		       dhcp_server_id |
		       dhcp_param_req_list |
		       dhcp_message |
		       dhcp_max_dhcp_message_size |
		       dhcp_renewal_time |
		       dhcp_rebinding_time |
		       dhcp_vendor_class_id |
		       dhcp_client_id |
		       dhcp_tftp_server |
		       dhcp_bootfile_size |
		       dhcp_end .

%% implement lease_db by yourself
-type dhcp_lease_type() :: ets | dets.

-type dhcp_lease_db() :: atom().

-type dhcp_lease_db_action() :: 
	alloc | release | update |
	challenge | aging.

-type dhcp_lease_flag() :: 
   avail | tmp | used | static .

-record(dhcp_packet, {
	  broadcast = broadcast :: dhcp_flags(),
	  message_type = -1 :: message_type(),
	  op      = 0 :: request,
	  htype   = 0 :: byte(),
	  hlen    = 0 :: byte(),
	  hops    = 0 :: byte(),
	  xid     = 0 :: uint32(),
	  secs    = 0 :: uint16(), 
	  flags   = unicast :: dhcp_flags(),
	  ciaddr  = {0,0,0,0} :: ip(),
	  yiaddr  = {0,0,0,0} :: ip(),
	  siaddr  = {0,0,0,0} :: ip(),
	  giaddr  = {0,0,0,0} :: ip(),
	  chaddr  = {0,0,0,0,0,0} :: ether(),
	  sname   = <<>> :: binary(),
	  file    = <<>> :: binary(),
	  options = [] :: [dhcp_option()]
	 }).

-record(dhcp_lease, {
	  ip_addr   = {0,0,0,0} :: ip(),
	  client_id = <<>> :: binary(),
	  mac  = {0,0,0,0,0} :: ether(),
	  flag      = 0 :: dhcp_lease_flag(),
	  updated   = 0 ::integer()
	 }).

-record(dhcp_lease_state, {
	  db :: dhcp_lease_db(),
	  range = [] :: list(),
	  expire = 0 :: integer()			
	 }).

-record(dhcp_config, {
	  options = {} :: map()	  
	 }).

-record(dhcp_args, {
	  config = "" :: string()
	 }).
	  
-record(lease_args, {
       name :: term(),
       path :: file:filename()
	 }).
