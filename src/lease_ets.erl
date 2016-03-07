-module(lease_ets).

-behavior(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

-include("dhcp.hrl").

%% API
-export([start_link/2, get_time/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(AGING_TIMER, 5000).
%%%=======================================================
%%% API function
%%%=======================================================
start_link(DBArgs, State) ->
    Name = DBArgs#lease_args.name,
    gen_server:start_link({local, Name}, ?MODULE, {DBArgs, State}, []).

%%%=======================================================
%%% gen_server callbacks
%%%=======================================================

init(Args) ->
    {DBArgs, State} = Args,
    Name = DBArgs#lease_args.name,
    ets:new(Name, [named_table, {keypos, 2}]),
    InsertRange = 
	fun(X) -> 
		Y = dhcpv4_lease_db:gen_lease_entries(X),
		ets:insert(Name, Y)
	end,
    lists:map(InsertRange, State#dhcp_lease_state.range),
    %% Aging Timer
    erlang:start_timer(?AGING_TIMER, self(), {self(), aging, {}}, []),
    {ok, State#dhcp_lease_state {
	   db = Name
	  }}.

handle_call({Type, Message}, _From, State) ->
    Reply = request(State, Type, Message),
    {reply, Reply, State}.

handle_cast(_Request, State) ->  
    {noreply, State}.

handle_info({timeout, _Ref, {Pid, Type, Request}}, State)
  when Pid == self() ->
    request(State, Type, Request),
    erlang:start_timer(?AGING_TIMER, self(), {self(), aging, {}}, []),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.
			      
terminate(_Reason, State) ->
    dets:close(State#dhcp_lease_state.db),
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
				    [{3, Request#dhcp_lease.client_id},
				     {4, Request#dhcp_lease.mac},
				     {5, tmp},
				     {6, Expire}]) of
		false ->
		    {error, "Failed to alloc element"};
		true ->
		    Results = ets:lookup(State#dhcp_lease_state.db,
					 IPAddr),
		    Result = hd(Results),
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
			       [{3, << >>},
				{4, {0,0,0,0,0,0}},
				{5, avail},
				{6, 0}])
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
    lists:map(ReleaseExpiredEntry, ExpiredEntries),
    
    EnableEntries =
	ets:select(
	  State#dhcp_lease_state.db,
	  ets:fun2ms(
	    fun (X = #dhcp_lease{flag = F})
		  when F == avail 
		       -> X
	    end
	   )),
    UsedEntries =
	ets:select(
	  State#dhcp_lease_state.db,
	  ets:fun2ms(
	    fun (X = #dhcp_lease{flag = F})
		  when F == used
		       -> X
	    end
	   )),
    lager:info("-- Lease information -- used: ~B, avail: ~B", 
	       [length(UsedEntries), length(EnableEntries)]),
    {ok, noreply}.   
    

%%%=======================================================
%%% misc function
%%%=======================================================
-spec get_time() -> integer().
get_time() ->
    {MgSec, Sec, _} = erlang:timestamp(),
    MgSec * 1000000 + Sec.

