.PHONY: deps

all: deps compile

compile:
	./rebar compile skip_deps=true

xref:
	./rebar xref skip_deps=true

generate:
	./rebar generate

deps:
	./rebar get-deps

unit:
	./rebar eunit

clean:
	./rebar clean

