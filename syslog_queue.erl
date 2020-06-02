-module(syslog_queue).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%%%
%%
%%%

-export([start_link/0, start/0, start_link/1, start/1
	]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("common.hrl").

-export([info/0,info/1]).

%%====================================================================
%% API
%%====================================================================
start()-> 
    {ok,Pid} = start_link(),
    %ok =
    %global:register_name(?MODULE,Pid),
    Pid.

start(N)-> 
    {ok,Pid} = start_link(N),
    %ok =
    %global:register_name(?MODULE,Pid),
    Pid.
    
start_link() ->
    logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "start_link" }),   
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(N) ->
    logger:?TRACE_POINTS_LEVEL(#{point => ?POINT ,reason => "start_link" , n => N}),   
    gen_server:start_link({local, list_to_atom(atom_to_list(?MODULE)++"_"++integer_to_list(N))}, ?MODULE, [], []).

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

handle_cast(_What, State) ->
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
    try do(Ask,State) of
     R->R
     catch Err:Reason -> ?CATCH_HANDLER({?MODULE,{catch_error,{do,{Ask,State}}}})
    end,   
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

do(Msg,State)->
  %logger:debug(#{workers => get(workers) }),
  % ret random worker from pool
  W = syslog_ns:pid(worker,rand:uniform(?WORKERS_NUM)),
  %logger:?TRACE_POINTS_LEVEL(#{worker_pid => W, point => ?POINT}),
  %W = syslog_ns:pid(worker,1+rand:uniform(erlang:system_time() rem ?WORKERS)),
  % W = choose_worker(Addr,Workers, fun(Add,Workers_) -> random_worker(Workers_) end),
  % send ask to worker's Pid
  R=W!{self(),Msg},
  logger:?TRACE_POINTS_LEVEL(#{ask => Msg, point => ?POINT_ARG(do) , reason => "passing message to worker was done" }),
  R.

info()-> lists:map(fun(A)-> {A,info(A)} end, [syslog_queue_1,syslog_queue_2]).

info(Queue)->
 whereis(Queue)!{self(),info},
 receive
  R->R
  after 5000 -> {timeout,Queue,info}
 end.

info(Queue,Subject)->
 whereis(Queue)!{self(),{info,Subject}},
 receive
  {Subject,Value} -> Value;
   R->R
  after 5000 -> {timeout,Queue,info}
 end.

%%====================================================================
%% Tests
%%====================================================================

%test() ->
%    ?assert(test(0) =:= 0),
%    ?assert(test(1) =:= 1),
%    ?assert(test(2) =:= 1).
