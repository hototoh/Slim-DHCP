.PHONY: 

all: compile

compile:
	./rebar3 compile 
	cd _build/default/lib/syslog/ && ./rebar compile && cd -

release:
	./rebar3 release

unit:
	./rebar3 eunit

clean:
	./rebar3 clean

