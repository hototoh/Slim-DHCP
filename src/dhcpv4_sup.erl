-module(dhcpv4_sup).
-behaviour(supervisor).

-include("dhcp.hrl").

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ======================================================
%% API functions
%% ======================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ======================================================
%% Supervisor callbacks
%% ======================================================

init(_Args) ->
    lager:info("Slim-DHCPv4 server launched."),
    {ok, Config} = dhcpv4_config:load(x),
    State = #dhcp_lease_state {
	       db     = node0,
	       range  = maps:get(range, Config#dhcp_config.options, err),
	       expire = maps:get(dhcp_ip_addr_lease_time, Config#dhcp_config.options, err)
	      },
    DBArgs = #lease_args {
           name = dhcpv4_lease,
           path = maps:get(dets_path, Config#dhcp_config.options, err)
    },
    {ok, { {one_for_one, 5, 10}, 
	   [
	    {dhcpv4, {dhcpv4_server, start_link, [node0, Config, dhcpv4_lease]}, permanent, 5000, worker, [dhcpv4_server]},
	    {dhcpv4_lease, {dhcpv4_lease_db, create, [dets, DBArgs, State]}, permanent, 5000, worker, [lease_dets]}
	   ]} }.
