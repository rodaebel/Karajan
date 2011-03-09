ERL=erl
ERLC=erlc
APP=karajan

all: compile

compile:
	@$(ERL) -pa ebin/ -make

clean:
	rm -f ebin/*.beam

docs:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' \
	'[{private, true}]'

clean-docs:
	rm -f doc/edoc-info doc/*.html doc/*.css doc/*.png

test: compile
	@$(ERL) -pa ebin -eval \
	"eunit:test({application,$(APP)}),eunit:test(karajan_zeroconf)" \
	-noshell -s init stop
