-module(dhcpv4_config).

-include("dhcp.hrl").

%% API
-export([load/1]).

-spec load(string()) -> {ok, dhcp:dhcp_config()}.
load(_FileName) ->
    {ok, #dhcp_config {
        options = maps:from_list(
            [
             {addr, {203,178,156,6}},
             {dets_path, "/var/db/dhcp/dhcpv4_lease"},
             {range, [
                  {{203,178,156,50}, {203,178,156,251}},
                  {{203,178,157,50}, {203,178,157,251}}
                    ]},
             {dhcp_server_id, {203,178,156,6}},
             {dhcp_subnet, {255,255,254,0}},
             {dhcp_domain_name_server, [
                            {203,178,158,53},
                            {203,178,158,54}
                           ]},
             {dhcp_router, [
                     {203,178,156,1}
                       ]},
             {dhcp_ip_addr_lease_time, 3600}
            ]
        ) } }.
