# ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
# ERLC_DIR = $(shell which erlc)
# ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))

compile:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -o ebin src/*.erl 

clean:
	@rm -Rf ebin

debug:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -Ddebug -o ebin src/*.erl 

test:
	@erl -noshell -pa ebin -eval "rev_erlang:start()." -s init stop
