REBAR = $(shell pwd)/rebar3
APP=sb_core

.PHONY: all ct test clean elvis compile

all: compile

clean:
	$(REBAR) cover -r
	$(REBAR) clean

eunit:
	$(REBAR) eunit
	$(REBAR) cover -v

ct:
	$(REBAR) ct
	$(REBAR) cover -v

dialyzer:
	$(REBAR) dialyzer

tests: elvis eunit ct
	$(REBAR) dialyzer

elvis:
	$(REBAR) lint

compile:
	$(REBAR) compile

rel:
	$(REBAR) release

run: rel
	./_build/default/rel/ct_gate/bin/ct_gate
shell:
	$(REBAR) shell
