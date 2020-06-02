{application, syslogd,
 [{description, "An OTP Syslod daemon"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { syslogd_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []},

  {maintainers, []},
  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
