ifndef ERLC
ERLC=erlc
endif

all: broadcast.beam cube.beam ytremote.beam

%.beam: %.erl
	$(ERLC) $<

clean:
	rm -f broadcast.beam cube.beam ytremote.beam
