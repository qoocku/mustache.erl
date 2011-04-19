all: mustache.erl

compile:
	@./rebar compile

clean:
	@./rebar clean

docs:
	@./rebar doc	 

mustache.erl: compile
	@./bootstrap
