-module(dhcp_app).

-behaviour(application).
-include("dhcp.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% =======================================================
%% Application callbacks
%% =======================================================

start(_StartType, _StartArgs) ->
    application:start(lager),    
    dhcp_sup:start_link().

stop(_State) ->
    application:stop(lager),
    ok.


-ifdef(TEST).

simple_test() ->
    ?debugHere,
    ok = application:start(dhcp),
    ?debugHere,
    {ok, Config} = dhcpv4_config:load(x),
    ?debugHere,
    ?debugHere,
    Tmp1 = lease_ets:request(eth0_lease, {alloc, #dhcp_lease{
						     client_id = <<1,24,5 >>
						   }}),
    ?debugHere,
    Tmp2 = lease_ets:request(eth0_lease, {alloc, #dhcp_lease{
						     client_id = << "ahocudcdasfd" >>
						    }}),
    lease_ets:request(eth0_lease, {update, #dhcp_lease{
					       ip_addr = Tmp1#dhcp_lease.ip_addr,
					       client_id = <<1,24,5 >>,
					       flag = used
					      }}),
    lease_ets:request(eth0_lease, {update, #dhcp_lease{
					       ip_addr = Tmp1#dhcp_lease.ip_addr,
					       client_id = <<1,24,5 >>,
					       flag = used
					      }}),
    lease_ets:request(eth0_lease, {update, #dhcp_lease{
					       ip_addr = Tmp2#dhcp_lease.ip_addr,
					       client_id = << "ahocudcdasfd" >>,
					       flag = used
					      }}),
    lease_ets:request(eth0_lease, {release, #dhcp_lease{
						ip_addr = Tmp1#dhcp_lease.ip_addr,
						client_id = <<1,24,5 >>
					       }}),
    lease_ets:request(eth0_lease, {release, #dhcp_lease{
						ip_addr = Tmp2#dhcp_lease.ip_addr,
						client_id = << "ahocudcdasfd" >>
					       }}).


%% lease_db_test() ->
%%     Range = [
%% 	     {{192,168,2,10}, {192,168,2,200}},
%% 	     {{192,168,3,10}, {192,168,3,200}}
%% 	    ],
%%     {ok, Config} = dhcpv4_config:load(x),
%%     State = #dhcp_lease_state {
%% 	       range = Range,
%% 	       expire = maps:get(default_lease_time, Config#dhcp_config.options, err)
%% 	      },
%%     register(leasedbtest, spawn_link(fun() -> dhcpv4_lease_db:create(ets, leasedbtest, State) end)),
%%     Tmp1 = lease_ets:request(leasedbtest, {alloc, #dhcp_lease{
%% 						     client_id = <<1,24,5 >>
%% 						   }}),
%%     Tmp2 = lease_ets:request(leasedbtest, {alloc, #dhcp_lease{
%% 						     client_id = << "ahocudcdasfd" >>
%% 						    }}),
%%     lease_ets:request(leasedbtest, {update, #dhcp_lease{
%% 					       ip_addr = Tmp1#dhcp_lease.ip_addr,
%% 					       client_id = <<1,24,5 >>,
%% 					       flag = used
%% 					      }}),
%%     lease_ets:request(leasedbtest, {update, #dhcp_lease{
%% 					       ip_addr = Tmp1#dhcp_lease.ip_addr,
%% 					       client_id = <<1,24,5 >>,
%% 					       flag = used
%% 					      }}),
%%     lease_ets:request(leasedbtest, {update, #dhcp_lease{
%% 					       ip_addr = Tmp2#dhcp_lease.ip_addr,
%% 					       client_id = << "ahocudcdasfd" >>,
%% 					       flag = used
%% 					      }}),
%%     lease_ets:request(leasedbtest, {release, #dhcp_lease{
%% 						ip_addr = Tmp1#dhcp_lease.ip_addr,
%% 						client_id = <<1,24,5 >>
%% 					       }}),
%%     lease_ets:request(leasedbtest, {release, #dhcp_lease{
%% 						ip_addr = Tmp2#dhcp_lease.ip_addr,
%% 						client_id = << "ahocudcdasfd" >>
%% 					       }}).
-endif.
