-module(syslogd).
-include("common.hrl").

-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(PORT,514).

%%%
%%
%%%

-export([start_link/0,
	 start/0]).

-export([init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).
	 
-export([init_workers/0,rand_node/1,info/0,info/1]).
%%====================================================================
%% API
%%====================================================================
start()-> logger:?TRACE_POINTS_LEVEL(?POINT_ARG(#{subject => start, 
                                       list => application:ensure_all_started(?MODULE)
                                      })
                         ),ok.

start_link() ->
    %syslog_queue:start_link(),
    logger:?TRACE_POINTS_LEVEL(#{ point => ?POINT_ARG(start_link) }),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Gen Server
%%====================================================================
            
init(Args) ->
% Init remore workers
%   lists:map(fun(N)-> syslog_ns:set(worker,N, do_spawn('workers@de.de',syslog_worker,loop_f2,[N,#{}])) end,lists:seq(1,?WORKERS_NUM)),
      init_workers(),
%             lists:map(fun(N)-> syslog_ns:set(worker,N,spawn(syslog_worker,loop_f2,[N,#{}])) end,lists:seq(1,WorkersNum)),
%             syslog_ns:set(server,do_spawn(node(),syslog_server,loop_f2,[1,#{}])),
%             logger:debug(#{point => ?POINT, servers => get(servers), workers => get(workers), socket => S, reason => "init server loop" }), 
%             Pid = syslog_ns:pid(server),
      {ok,S}=case inet_udp:open(?PORT,[binary,
                            {active,true},
                            {reuseaddr,true}
                           ]) 
      of
       R->logger:?TRACE_POINTS_LEVEL(#{listener => R, point => ?POINT_ARG(handle_call) }),
          R
      end, 
    {ok, #{listener => S, args => Args}}.

handle_call({Ask, Arg}, _From, State) ->
    logger:?TRACE_POINTS_LEVEL(#{ask => Ask, arg => Arg, point => ?POINT_ARG(handle_call) }),
    {reply, {ok, "Reply"}, State};

handle_call(What, _From, State) ->
    logger:?TRACE_POINTS_LEVEL(#{ask => What, point => ?POINT_ARG(handle_call) , reason => unexpected }),
    {reply, {error, What}, State}.

handle_cast(What, State) ->
     logger:?TRACE_POINTS_LEVEL(#{ask => What, point => ?POINT_ARG(handle_cast) , reason => unexpected }),
     {noreply, State}.

% handle asks about listener info
handle_info({Pid,info}, State) ->
    logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "handle_info {Pid,info} ask" , ask => info, state => State }),
    {ok,SocketStat} = inet:getstat(maps:get(listener,State)),
    Pid!#{process_info => erlang:process_info(self()), socket_stat => SocketStat, state => State, socket => maps:get(listener,State)},  
    {noreply, State};

handle_info({Pid,{info,Subject}}, State) ->
    logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "handle_info {Pid,info} ask" , ask => info, state => State }),
    Pid!erlang:process_info(self(),Subject),  
    {noreply, State};
    
% handle incoming UDP traffic
handle_info({udp,Sock,
              IP,
              Port,Msg}, State) ->
     logger:?TRACE_POINTS_LEVEL(#{ask => {IP,Port,Msg}, point => ?POINT_ARG(handle_info) , reason => handle_message }),
     % without queue syslogd_handler:do({IP,Port,Msg},State),
     %syslog_queue_1!{udp,Sock,IP,Port,Msg},
     %works 
     try syslog_queue_1!{udp,Sock,IP,Port,Msg} of
      R-> R %logger:critical(#{point => ?POINT_ARG({?MODULE,?LINE,R})})
     catch
      Err:Reason -> syslog_queue_2!{udp,Sock,IP,Port,Msg},
                    logger:critical(#{point => ?POINT_ARG({?MODULE,?LINE,"was done routing for message to hot spare syslog_queue_2"})}),
                    ?CATCH_HANDLER({{?MODULE,?LINE},{handle_info,{udp,Sock,IP,Port,Msg}}})
     end,
     %global:whereis_name(syslog_queue)!{udp,Sock,IP,Port,Msg},
     {noreply, State};
     
% handle unexpected asks
handle_info(What, State) ->
     logger:?TRACE_POINTS_LEVEL(#{ask => What, point => ?POINT_ARG(handle_info) , reason => unexpected }),
     {noreply, State}.

terminate(Reason, _State) ->
    logger:?TRACE_POINTS_LEVEL(#{ask => Reason, point => ?POINT_ARG(terminate) , reason => unexpected }),
    ok.

code_change(OldVsn, State, Extra) ->
    logger:?TRACE_POINTS_LEVEL(#{ask => {OldVsn, State, Extra}, point => ?POINT_ARG(code_change) , reason => unexpected }),
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
% depricated and need to remove to syslog_handler

do_spawn(Node,M,F,A) -> %spawn(M,F,A).
                   %rpc:call(node(),code,load_file,[M]),
                   %rpc:call(node(),M,F,A).
                   
                   %rpc:call(node(),code,load_file,[M]),
                   
                   R=rpc:call(Node,erlang,spawn,[M,F,A]),
                   logger:critical(#{point => ?POINT, rpc_at_node => Node, mfa =>{M,F,A}, result => R, reason =>{rpc_call,Node,M,F,A}}),
                   R.

rand_node(WORKERS_NODES)-> lists:nth(rand:uniform(length(WORKERS_NODES)),WORKERS_NODES).

init_workers()->
             lists:map(fun(Nod)-> net_adm:ping(Nod) end, ?WORKERS_NODES),
             %lists:map(fun(Nod)-> net_adm:ping(Nod) end, syslog_ns:cluster()),
             
             logger:?TRACE_POINTS_LEVEL(#{point => ?POINT, nodes => [node()|nodes()], names => net_adm:names() ,reason => "init workers" }),   

% non OTP workers
%             lists:map(fun(N)-> syslog_ns:set(worker,N, do_spawn(rand_node(?WORKERS_NODES
%                                                                           %syslog_ns:cluster()
%                                                                          ),
%                                                                 syslog_worker,
%                                                                 loop_f2,
%                                                                 [N,#{}]
%                                                                )
%                                             ) 
%                           end,
%                           lists:seq(1,?WORKERS_NUM)
%                          ).

             lists:map(fun(N)-> syslog_ns:set(worker,N, do_spawn(rand_node(?WORKERS_NODES
                                                                           %syslog_ns:cluster()
                                                                          ),
                                                                 syslog_workers,
                                                                 start_link,
                                                                 [N]
                                                                )
                                             ) 
                           end,
                           lists:seq(1,?WORKERS_NUM)
                          ).
                          
% run worker_1 as {ok,Pid}=rpc:call(Node,syslog_workers,start_link,[N])

info()->
 ?MODULE!{self(),info},
 receive
  R->R
  after 5000 -> {timeout,?MODULE,info}
 end.

info(Subject)->
 ?MODULE!{self(),info,Subject},
 receive
  {Subject,R}->R
  after 5000 -> {timeout,?MODULE,info,Subject}
 end.
 