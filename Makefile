.PHONY: rel deps test
INCLUDE_FILES=$(shell escript tools/get_included_files_h.erl)
CFLAGS_LINUX = -shared
CFLAGS_DARWIN = -undefined dynamic_lookup
CFLAGS_REST = -lexpat -fPIC
ifeq ($(shell uname), Darwin)
	CFLAGS = $(CFLAGS_DARWIN) $(INCLUDE_FILES) $(CFLAGS_REST)
else
	CFLAGS = $(CFLAGS_LINUX) $(INCLUDE_FILES) $(CFLAGS_REST)
endif
SO = priv/librxml.so
SO_TARGET ?= debug

all: deps compile

compile: rebar
	./rebar compile

deps: rebar
	./rebar get-deps

clean: rebar
	./rebar clean

test: compile
	./rebar skip_deps=true eunit

rebar:
	wget https://github.com/rebar/rebar/releases/download/2.5.1/rebar && chmod u+x rebar

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps kernel stdlib erts; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/exml.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/exml.plt \
	-o dialyzer/exml.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

exml_plt: dialyzer/exml.plt
	@dialyzer --plt dialyzer/exml.plt --check_plt -o dialyzer/exml.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt exml_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log ebin

shared_libs: $(SO)

ifeq ($(shell uname), Darwin)
PLATFORM_SO := dylib
else
PLATFORM_SO := so
endif

priv/librxml.so: rust_src/target/$(SO_TARGET)/librxml.$(PLATFORM_SO)
	@mkdir -p priv
	cp $< $@


ifeq ($(shell uname), Darwin)
L_ERL_INTERFACE := $(wildcard $(dir $(subst /bin/erl,,$(shell which erl)))/lib/erl_interface-*)/lib
rust_src/target/$(SO_TARGET)/librxml.$(PLATFORM_SO): $(wildcard rust_src/src/*.rs)
	cd rust_src && \
		cargo rustc -- -L $(L_ERL_INTERFACE) \
			-l erl_interface -l ei \
			-C link-args='-flat_namespace -undefined suppress' > build.log 2>&1
else
rust_src/target/$(SO_TARGET)/librxml.$(PLATFORM_SO): $(wildcard rust_src/src/*.rs)
	cd rust_src && cargo build > build.log 2>&1
endif

shared_clean:
	-rm $(SO)
	-rm -r priv
