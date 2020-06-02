-module(syslogd_handler).
-compile(export_all).
-include("common.hrl").
% depricated by reason of replacing it to queue server in OTP style
 
% get message and pass it to some worker
%do({IP,Port,Msg},State)->
do(Msg,State)->
  logger:debug(#{workers => get(workers) }),
  % ret random worker from pool
  W = syslog_ns:pid(worker,rand:uniform(?WORKERS_NUM)),
  %W = syslog_ns:pid(worker,1+rand:uniform(erlang:system_time() rem ?WORKERS)),
  % W = choose_worker(Addr,Workers, fun(Add,Workers_) -> random_worker(Workers_) end),
  % send ask to worker's Pid
  R=W!{self(),Msg},
  logger:?TRACE_POINTS_LEVEL(#{ask => Msg, point => ?POINT_ARG(do) , reason => handle_message }),
  R.

   