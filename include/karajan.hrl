-record(server_state, {socket=null}).

-record(clock_state, {run=false, proc=null, port=null, socket=null}).

-record(zeroconf_state, {port=null, socket=null}).
