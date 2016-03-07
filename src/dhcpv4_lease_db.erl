-module(dhcpv4_lease_db).

-include("dhcp.hrl").

%% API
-export([create/3, request/2]).
%% utils
-export([gen_lease_entries/1]).

-spec create(dhcp_lease_type(),
	     atom(), 
	     dhcp:dhcp_lease_state())
	    -> {ok, pid()} | ignore | {error, term()}.
create(DBType, DBArgs, State) ->
    case DBType of
	ets ->
	    lease_ets:start_link(DBArgs, State);
	dets ->
	    lease_dets:start_link(DBArgs, State);
	_ ->
	    lager:error("Invalid LeaseDB type~n"),
	    {error, "Invalid LeaseDB type"}
    end.

-spec request(dhcp:dhcp_lease_state(), tuple())
	     -> tuple().
request(DB, Request) ->
    {Type, Message} = Request,
    Reply = gen_server:call(DB, {Type, Message}),
    case Reply of
	{ok, Ret} ->
	    Ret;
	{error, Reason} ->
	    {error, Reason}
    end.

    
-spec ip_to_integer(dhcp:ip_addr()) -> integer().
ip_to_integer(IP) ->
    {A, B, C, D} = IP,
    (A bsl 24) +  (B bsl 16) + (C bsl 8) + D.

-spec integer_to_ip(integer()) -> dhcp:ip().
integer_to_ip(IntIP) ->
    {(IntIP bsr 24) band 16#ff,
     (IntIP bsr 16) band 16#ff,
     (IntIP bsr 8) band 16#ff,
     IntIP band 16#ff}.

-spec ip_range_to_list(dhcp:ip_pair()) -> list().
ip_range_to_list({StartIP, EndIP}) ->
    Start = ip_to_integer(StartIP),
    End   = ip_to_integer(EndIP),
    lists:map(fun(X) -> integer_to_ip(X) end,
	      lists:seq(Start, End)).

-spec gen_lease_entries(dhcp:ip_pair()) -> list().
gen_lease_entries(Range) ->
    IPLists = ip_range_to_list(Range),
    lists:map(fun(X) ->
		      #dhcp_lease {
			 ip_addr = X,
			 flag = avail
			}
	      end, IPLists).
		       
