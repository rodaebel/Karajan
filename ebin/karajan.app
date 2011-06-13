%% -*-Erlang-*-
{application, karajan,
 [{description, "Karajan"},
  {vsn, "1.0.0"},
  {modules, [karajan, karajan_app, karajan_sup, karajan_server,
             karajan_handler]},
  {registered, [karajan_sup]},
  {applications, [kernel, stdlib]},
  {mod, {karajan_app, []}}
 ]}.
%% vim: set filetype=erlang :
