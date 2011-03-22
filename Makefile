ERL=erl
ERLC=erlc
APP=karajan

all: compile
	@cd lib/Tosca; $(MAKE)

compile:
	@$(ERL) -pa ebin/ -make

clean:
	@cd lib/Tosca; $(MAKE) clean
	rm -f ebin/*.beam

docs:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' \
	'[{private, true}]'

clean-docs:
	rm -f doc/edoc-info doc/*.html doc/*.css doc/*.png

test: compile
	@$(ERL) -pa ebin -eval \
	"eunit:test({application,$(APP)})" \
	-noshell -s init stop
