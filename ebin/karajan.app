%% -*-Erlang-*-
{application, karajan,
 [{description, "Open Sound Control Server/Dispatcher"},
  {vsn, "1.0.0"},
  {modules, [karajan_app, karajan_sup, karajan_server, karajan_handler,
             karajan_guard, karajan_clock, osc_lib]},
  {registered, [karajan_sup, karajan_server, karajan_guard, karajan_clock]},
  {applications, [kernel, stdlib]},
  {mod, {karajan_app, []}},
  {env, [{ip, any}, {incoming_port, 7000}, {outgoing_port, 7124}, {recbuf, 8192}]}
 ]}.
%% vim: set filetype=erlang :
