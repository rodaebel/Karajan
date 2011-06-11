ifndef ERLC
ERLC=erlc
endif

all: cube.beam ytremote.beam

%.beam: %.erl
	$(ERLC) $<

clean:
	rm -f cube.beam ytremote.beam
