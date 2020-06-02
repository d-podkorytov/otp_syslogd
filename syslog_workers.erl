-module(syslog_workers).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%%%
%%
%%%

-export([start_link/0, start_link/1, start/2, start/1, 
         info/0, info/1, info/2
	]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("common.hrl").

%%====================================================================
%% API
%%====================================================================
start(N)-> 
     P= case start_link() of
     {ok,Pid}                -> Pid;
     {error,{already_started,Pid2}} -> Pid2; 
      R-> logger:critical(#{point => ?POINT, return => R, reason => unexpected}),
          R
end.
%    logger:critical(#{point => ?POINT, reason => "try to start worker", no => N ,pid => P}),
%    %yes = 
%    global:register_name(list_to_atom("worker_"++integer_to_list(N)),P),
%    P.
% do not used and debugged yet
start(Node,N)->
     % or use pg 
    {ok,Pid} = rpc:call(Node,?MODULE,start_link,[]),
    logger:critical(#{point => ?POINT, reason => "try to start worker", no => N, node => Node}),
    %yes = 
    global:register_name(list_to_atom("worker_"++integer_to_list(N)),Pid),
    Pid.
    
start_link() ->
    logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "start_link" }),   
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(N) ->
    logger:?TRACE_POINTS_LEVEL(#{no => N,point => ?POINT ,reason => "start_link" }),   
    gen_server:start_link({global, list_to_atom("worker_"++integer_to_list(N)) }, ?MODULE, [], []).

%%====================================================================
%% Gen Server
%%====================================================================

init(Arg) ->
    logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "init" , state => Arg}),
    %register(?MODULE,self()),   
    {ok, #{pid => self() }}.

handle_call(Ask, _From, State) ->
	logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "handle_call" , ask => Ask, state => State }),   
	{reply, {ok, {?MODULE,?LINE,'Result'}}, State};

handle_call(What, _From, State) ->
	{reply, {error, What}, State}.

handle_cast(Ask, State) ->
	logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "handle_cast" , ask => Ask, state => State }),   
	{noreply, State}.

%-include("process_info.hrl").

handle_info({Pid,info}, State) ->
    logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "handle_info {Pid,info} ask" , ask => info, state => State }),
    Pid!#{process_info => erlang:process_info(self()) },  
    {noreply, State};

handle_info({Pid,{info,Subject}}, State) ->
    logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "handle_info {Pid,info} ask" , ask => info, state => State }),
    Pid!erlang:process_info(self(),Subject),  
    {noreply, State};

handle_info(Ask, State) ->
    logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "handle_info" , ask => Ask, state => State }),
    route(Ask,State),   
    {noreply, State}.

terminate(Reason, State) ->
    logger:critical(#{point => ?POINT ,reason => "terminate" , ask => Reason, state => State }),   
    ok.

code_change(OldVsn, State, Extra) ->
    logger:critical(#{point => ?POINT ,reason => "code_change" , ask => OldVsn, state => State , extra => Extra}),   
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

route(Msg,State)->
  % route Msg to different endpoints messages will be here
  logger:critical(#{ask => Msg, 
                    point => ?POINT_ARG("route Msg to different endpoints messages will be here"), 
                    reason => "try to route message" }).

info(Worker)->
 global:whereis_name(Worker)!{self(),info},
 receive
  R->R
  after 10000 -> {timeout,Worker,info}
 end.

info(Worker,Subject)->
 global:whereis_name(Worker)!{self(),{info,Subject}},
 receive
  {Subject,Value} -> Value;
   R->R
  after 10000 -> {timeout,Worker,info}
 end.

info()->
 lists:map(fun(N)->
                 Worker =  list_to_atom("worker_"++integer_to_list(N)),
                #{no     => N, 
                  pid    => global:whereis_name(Worker), 
                  info   => info(Worker), 
                  worker => Worker
                 } 
           end,
           lists:seq(1,?WORKERS_NUM)
          ).
 
%%====================================================================
%% Tests
%%====================================================================

%test() ->
%    ?assert(test(0) =:= 0),
%    ?assert(test(1) =:= 1),
%    ?assert(test(2) =:= 1).
