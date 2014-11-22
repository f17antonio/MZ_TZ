REPO       ?= mztz
TAG         = $(shell git describe --tags)
REVISION   ?= $(shell echo $(TAG) | sed -e 's/^$(REPO)-//')
VERSION    ?= $(shell echo $(REVISION) | tr - .)
EPMD        = $(shell pgrep epmd)tail
TAIL        = tail

REBAR=./rebar

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

generate:
	@$(REBAR) generate

regenerate:
	@$(REBAR) compile generate skip_deps=true

install:
	@$(REBAR) get-deps compile generate

unit-test:
	@$(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean

distclean: clean relclean
	@$(REBAR) delete-deps

docs:
	@$(REBAR) skip_deps=true doc

start:
	@./rel/mztz/bin/mztz start

attach:
	@./rel/mztz/bin/mztz attach

stop:
	@./rel/mztz/bin/mztz stop
