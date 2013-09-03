APP_DEPS=kernel stdlib eunit tools compiler
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

REBAR="./rebar"
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

ERL = $(shell which erl)
ifeq ($(ERL),)
$(error "Erlang must be available on this system")
endif

.PHONY: all rebuild compile clean test get-deps clean-deps \
	shell distclean

all: get-deps compile

rebuild: distclean get-deps all

get-deps:
	@$(REBAR) -C rebar.config get-deps
	@$(REBAR) -C rebar.config compile

compile:
	@$(REBAR) -C rebar.config skip_deps=true compile

clean:
	@$(REBAR) -C rebar.config skip_deps=true clean

#test: get-deps
#	@$(REBAR) -C rebar.config skip_deps=true eunit

shell:
	@$(ERL) $(ERLFLAGS)

clean-deps:
	@rm -rvf $(CURDIR)/deps/*

distclean: clean clean-deps
