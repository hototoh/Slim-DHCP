# Slim-DHCP

## About
This is an implementation of DHCP Server by Erlang. 

* Support DHCPv4
* Run only on Linux.
	* FreeBSD behave differently to send to limited broadcast address. it route the packet to the default router. we cannot fix this, because we doesn't use raw socket.
* Multiple DataStore
	* ETS, DETS
	* If you want to use another datastore, you need to implement but it is very easy to add. See lease_dets.erl and dhcpv4_lease_db.erl files in more detail.
	

## Releases
- version 0.1
	* no configuration file version.
	* not support static dhcp
	


## Build
	$ make compile
	$ make release
	


	