{application, cccp,
 [
  {description, "Skel - A cccpeton application useful for quickly creating services"},
  {vsn, "0.0.1"},
  {modules, [cccp_app, cccp, cccp_handlers, cccp_listener, cccp_sup]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { cccp_app, []}},
  {env, []}
 ]}.
