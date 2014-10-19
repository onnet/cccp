{application, cccp,
 [
  {description, "CCCP - Calling Card Callback Platform"},
  {vsn, "0.0.1"},
  {modules, [cccp_app, cccp_callback_handler, cccp_callback_sup, cccp, cccp_handlers, cccp_listener, cccp_sup, cccp_util]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { cccp_app, []}},
  {env, []}
 ]}.
