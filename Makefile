REBAR = @$(shell pwd)/rebar
APP_NAME = guinea

all: xref

dependencies:
	@${REBAR} get-deps

applications: dependencies
	@${REBAR} compile

test:
	@${REBAR} eunit skip_deps=true

xref: applications
	@${REBAR} skip_deps=true xref

dialyzer:
	@dialyzer --plt .@${APP_NAME}.plt -r apps \
		-Wrace_conditions -Wunderspecs -Wspecdiffs

plt: applications
	@dialyzer --output_plt .@${APP_NAME}.plt --build_plt --apps \
		erts kernel stdlib crypto sasl ssl inets xmerl public_key compiler \
		tools runtime_tools deps/*/ebin

# Cleaning tasks:

depclean:
	@rm -rf deps/*

clean:
	$(REBAR) clean skip_deps=true

allclean: depclean
	@${REBAR} clean

# Other tasks:

start: applications
	erl -pa deps/*/ebin ebin -boot start_sasl \
		-eval "app_util:dev_start(@${APP_NAME}, permanent)."

fastcompile:
	@$(REBAR) skip_deps=true compile

faststart: fastcompile
	erl -pa deps/*/ebin ebin -boot start_sasl \
		-eval "app_util:dev_start(@${APP_NAME}, permanent)."

.PHONY: all test dialyzer clean allclean dependencies start
