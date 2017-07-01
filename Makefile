
compile: clean
	@mkdir ebin
	@erlc -o ebin src/*.erl
	@$(MAKE) script

script:
	@echo "erl -noshell -pa ebin -eval \"rev_erlang:start().\" -s init stop" > rev-erlang.sh
	@chmod +x rev-erlang.sh

clean:
	@rm -Rf ebin

debug: clean
	@mkdir ebin
	@erlc -Ddebug -o ebin src/*.erl
	@$(MAKE) script
