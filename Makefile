REBAR=$(shell which rebar || echo ./rebar)

all: build

clean:
	$(REBAR) clean
	rm -rf .eunit
	rm -f test/*.beam
	rm -f test/Emakefile

compile:
	@$(REBAR) compile

distclean: clean
	git clean -fxd

build:
	$(REBAR) compile

eunit:
	@$(REBAR) skip_deps=true eunit

test: eunit

check: build eunit

%.beam: %.erl
	erlc -o test/ $<

.PHONY: all clean distclean depends build etap eunit check
