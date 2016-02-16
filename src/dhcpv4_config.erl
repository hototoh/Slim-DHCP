-module(dhcpv4_config).

-include("dhcp.hrl").

%% API
-export([load/1]).

-spec load(string()) -> {ok, dhcp:dhcp_config()}.
load(_FileName) ->
    {ok, #dhcp_config {
	    options = maps:from_list(
			[
			 {addr, {192,168,1,1}},
			 {range, [
				  {{192,168,1,10}, {192,168,1,100}},

				  {{192,168,1,110}, {192,168,1,200}}]},
			 {default_lease_time, 600},
			 {dhcp_server_id, {192,168,1,1}},
			 {dhcp_subnet, {255,255,255,0}},
			 {dhcp_domain_name_server, [
						    {192,168,1,2},
						    {192,168,2,2}
						   ]},
			 {dhcp_router, [ 
					 {192,168,1,1},
					 {192,168,2,1}
				       ]},
			 {dhcp_ip_addr_lease_time, 60}
			]
		       ) } }.
