.PHONY: myrlang

myrlang:
	rebar3 escriptize
	cp _build/default/bin/myrlang myrlang
