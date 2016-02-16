.PHONY: deps

all: deps compile

compile:
	rebar compile

deps:
	rebar get-deps

unit:
	rebar eunit

clean:
	rebar clean

