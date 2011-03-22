%% -*-Erlang-*-
{application, karajan,
 [{description, "Karajan"},
  {vsn, "1.0.0"},
  {modules, [karajan_app, karajan_sup, karajan_handler, karajan_clock]},
  {registered, [karajan_sup, karajan_clock]},
  {applications, [kernel, stdlib]},
  {mod, {karajan_app, []}}
 ]}.
%% vim: set filetype=erlang :
