-module(lease_ets).

-behavior(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("dhcp.hrl").

%% API
-export([start_link/3,request/2, get_time/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(AGING_TIMER, 5000).
%%%=======================================================
%%% API function
%%%=======================================================
-spec request(dhcp:dhcp_lease_state(), tuple())
	     -> tuple().
request(DB, Request) ->
    {Type, Message} = Request,
    whereis(DB) ! {self(), Type, Message},
    receive 
	{ok, Ret} ->
	    Ret;
	{error, Reason} ->
	    {error, Reason}
    end.

start_link(Name, Logger, State) ->
    io:format("start_link, dhcpv4_lease: ~w ~w~n", [Name, State]),
    gen_server:start_link({local, Name}, ?MODULE, {Name, Logger, State}, []).

%%%=======================================================
%%% gen_server callbacks
%%%=======================================================

init(Args) ->
    {Name, Logger, State} = Args,
    ets:new(Name, [named_table, {keypos, 2}]),
    InsertRange = 
	fun(X) -> 
		Y = dhcpv4_lease_db:gen_lease_entries(X),
		ets:insert(Name, Y)
	end,
    lists:map(InsertRange, State#dhcp_lease_state.range),
    %% Aging Timer
    erlang:send_after(?AGING_TIMER, self(), {self(), aging, {}}, []),
    {ok, State#dhcp_lease_state {
	   logger = Logger,
	   db = Name
	  }}.

handle_call(_Request, _From, State) ->
    {reply, State}.

handle_cast(_Request, State) ->  
    {noreply, State}.

handle_info({timeout, _Ref, {_Pid, Type, Request}}
	   , State) ->
    request(State, Type, Request),
    {noreply, State};
handle_info({Pid, Type, Request}, State) ->
    Pid ! request(State, Type, Request),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
			      
terminate(_Reason, _) ->
    {ok, normal}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=======================================================
%%% private function
%%%=======================================================
-spec request(dhcp:dhcp_lease_state(),
	      dhcp_lease_db_action(),
	      dhcp:dhcp_lease()) -> tuple().
request(State, alloc, Request) ->
    EnableEntries =
	ets:select(
	  State#dhcp_lease_state.db,
	  ets:fun2ms(
	    fun (X = #dhcp_lease{flag = F})
		  when F == avail 
		       -> X
	    end
	   )),
    case EnableEntries of
	[] -> 
	    {error, "alloc: no enable entry"};
	_ ->
	    TmpEntry    = hd(EnableEntries),
	    Expire = lease_ets:get_time() + State#dhcp_lease_state.expire,
	    IPAddr = case Request#dhcp_lease.ip_addr of
			 {0,0,0,0} ->
			     TmpEntry#dhcp_lease.ip_addr;
			 _ ->
			     Request#dhcp_lease.ip_addr
		     end,
	    case ets:update_element(State#dhcp_lease_state.db,
				    IPAddr,
				    [{3, Request#dhcp_lease.mac},
				     {4, Request#dhcp_lease.client_id},
				     {5, tmp},
				     {6, Expire}]) of
		false ->
		    {error, "Failed to alloc element"};
		true ->
		    Results = ets:lookup(State#dhcp_lease_state.db,
					 IPAddr),
		    Result = hd(Results),
		    %% gen_server:cast(State#dhcp_lease_state.logger, {alloc, Result, Expire}),
		    {ok, Result}		   
	    end
    end;
request(State, release, Request) ->
    UsedEntries =
	ets:select(
	  State#dhcp_lease_state.db,
	  ets:fun2ms(
	    fun (X = #dhcp_lease{ip_addr = IPAddr, 
				 client_id = ClientId})
		  when IPAddr == Request#dhcp_lease.ip_addr
		       andalso
		       ClientId == Request#dhcp_lease.client_id
		       -> X
	    end
	   )),
    case UsedEntries of
	[] -> 
	    {error, "release: Not found entry"};	
	_ ->	    
	    TmpEntry = hd(UsedEntries),
	    ets:update_element(State#dhcp_lease_state.db,
			       TmpEntry#dhcp_lease.ip_addr,
			       [{3, {0,0,0,0,0,0}},
				{4, << >>},
				{5, avail},
				{6, 0}])
	    %% gen_server:cast(State#dhcp_lease_state.logger, {alloc, TmpEntry, 0}), {ok, { }}
    end;
request(State, update, Request) ->
    UsedEntries = 
	ets:select(
	  State#dhcp_lease_state.db,
	  ets:fun2ms(
	    fun (X = #dhcp_lease{ip_addr = IPAddr, 
				 client_id = ClientId})
		  when IPAddr == Request#dhcp_lease.ip_addr
		       andalso
		       ClientId == Request#dhcp_lease.client_id
		       -> X
	    end
	   )),
    case UsedEntries of
	[] -> 
	    {error, "update: Not found entry"};	
	_ ->	    
	    TmpEntry = hd(UsedEntries),
	    Expire = lease_ets:get_time() + State#dhcp_lease_state.expire,
	    ets:update_element(State#dhcp_lease_state.db,
			       TmpEntry#dhcp_lease.ip_addr,
			       [{5, Request#dhcp_lease.flag},
				{6, Expire }]),
	    %% gen_server:cast(State#dhcp_lease_state.logger, {update, TmpEntry, Expire}),
	    {ok, TmpEntry#dhcp_lease {
		   flag = Request#dhcp_lease.flag,
		   updated = Expire
		  }}
    end;
request(State, challenge, Request) ->
    Entries = ets:lookup(
	      State#dhcp_lease_state.db,
	      Request#dhcp_lease.ip_addr),
    
    case Entries of
	[] -> 
	    request(State, alloc, Request);
	_ ->	    
	    Entry = hd(Entries),
	    case Entry#dhcp_lease.flag of
		avail ->
		    request(State, alloc,
			    Request#dhcp_lease{
			      flag = tmp
			     });
		_ ->
		    request(State, alloc, Request)
	    end
    end;
request(State, aging, _Request) ->
    Expire = lease_ets:get_time(),
    ExpiredEntries = 
	ets:select(
	  State#dhcp_lease_state.db,
	  ets:fun2ms(
	    fun (X = #dhcp_lease{updated = U})
		  when U =< Expire andalso
		       U /= 0
		       -> X
	    end
	   )),
    ReleaseExpiredEntry = 
	fun (X) ->
		request(State, release, X)
	end,
    erlang:start_timer(?AGING_TIMER, self(), {self(), aging, {}}, []),
    lists:map(ReleaseExpiredEntry, ExpiredEntries).
    

%%%=======================================================
%%% misc function
%%%=======================================================
-spec get_time() -> integer().
get_time() ->
    {MgSec, Sec, _} = erlang:timestamp(),
    MgSec * 1000000 + Sec.

