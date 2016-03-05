.PHONY: deps

all: deps compile

compile:
	rebar compile skip_deps=true

xref:
	rebar xref skip_deps=true

deps:
	rebar get-deps

unit:
	rebar eunit

clean:
	rebar clean

