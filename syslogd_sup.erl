%%%-------------------------------------------------------------------
%% @doc syslogd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(syslogd_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


-include("common.hrl").
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    Listener = { syslogd,{syslogd, start_link, []},permanent, 5000, worker, [syslogd]},
    Queue_1 = { syslog_queue_1,{syslog_queue, start_link, [1]},permanent, 5000, worker, [syslog_queue_1]},
    Queue_2 = { syslog_queue_2,{syslog_queue, start_link, [2]},permanent, 5000, worker, [syslog_queue_2]},

    %{ok,Queue} = syslog_queue:start_link(),
    %supervisor:start_child(?MODULE,Queue),
    logger:critical(#{point => ?POINT, queues => [Queue_1,Queue_2]}),     
    {ok, { {one_for_all, 10, 10}, [Listener,Queue_1,Queue_2
                                  ]
         } 
    }.

%%====================================================================
%% Internal functions
%%====================================================================
