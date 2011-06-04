ERLC=erlc

all: ../ebin/cube.beam ../ebin/ytremote.beam

../ebin/%.beam: %.erl
	$(ERLC) -o ../ebin $<

clean:
	rm -f ../ebin/cube.beam ../ebin/ytremote.beam
