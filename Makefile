all: mustache.erl

compile:
	@./rebar compile

clean:
	@./rebar clean

docs:
	@./rebar doc	 

get-deps:
	@./rebar get-deps

mustache.erl: get-deps compile
	@./bootstrap
