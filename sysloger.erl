% simple Syslog client
% send some message to syslog server
-module(sysloger).
-compile(export_all).
-include("common.hrl").

-define(PREAMBLE,"<165>:Jun 21 16:13:28 UTC:").

ask(Query)      ->a({127,0,0,1},514,Query).
ask(Host,Query) ->a(Host,514,Query).
ask(Host,Port,Query) ->a(Host,Port,Query).

a(Host,Port,Query)->
                      {ok, S} = gen_udp:open(0, [binary, {active, false}, {recbuf, 8192}]),
                      ok = gen_udp:connect(S, Host, Port),
                      gen_udp:send(S,Query),
                      gen_udp:close(S).

tests()-> [ask("<165>:Jun 21 16:13:28 UTC: Test"),
          log("Test"),
          log(0,0,"Test 1 1"),
          log(1,2,"Test 1 2"),
          log(2,2,"Test 2 2"),
          log(1,1,"Test 0 0"),
          log(256,256,"Test 256 256")
         ].

test()-> log(1,1,"Tests").

started()-> log(1,1,#{ reason => started, 
                       pid => self() , 
                       node => node() , 
                       local_time => calendar:local_time() , 
                       point => ?POINT 
                     }
               ).

stoped()-> log(1,1,#{ reason => stoped, 
                       pid => self() , 
                       node => node() , 
                       local_time => calendar:local_time() , 
                       point => ?POINT 
                     }
               ).

start()-> start({127,0,0,1},514).

start(Host,Port)->
          {ok, S} = gen_udp:open(0, [binary, {active, true}, {recbuf, 8192}]),
           ok     = gen_udp:connect(S, Host, Port), 
          Pid=spawn(fun()-> loop(S) end),
          register(?MODULE,Pid),
          ok.

% client loop 
loop(S)->
 receive
  {From,Msg}-> gen_udp:send(S,Msg) 
 end,
 loop(S).          

% store msg
send(Msg)->
 ?MODULE!{self(),Msg}.           

log(Msg)     -> log(20,1,Msg).

log(F,S,Msg) -> io:format("~p:~p ~p ~p ~n",[?MODULE,?LINE,date_(),Msg]),
                ask(io_lib:format("~s ~s ~p ~n",[preamble(F,S),date_(),Msg])).

date_()->txt_date(calendar:universal_time())++" UTC: ".

txt_date({{Year,Mo,Day},{H,M,S}})->month_name(Mo)++" "++integer_to_list(Day)++" "++
                                   integer_to_list(H)++":"++integer_to_list(M)++":"++integer_to_list(S).

month_name(1) ->"Jan";
month_name(2) ->"Feb";
month_name(3) ->"Mar";
month_name(4) ->"Apr";
month_name(5) ->"May";
month_name(6) ->"Jun";
month_name(7) ->"Jul";
month_name(8) ->"Aug";
month_name(9) ->"Sep";
month_name(10)->"Oct";
month_name(11)->"Nov";
month_name(12)->"Dec".

fs2int(F,S) -> <<D>>       = <<F:5,S:3>>, D.
int2fs(Int) -> <<F:5,S:3>> = <<Int:8>>  , {F,S}.

preamble(F,S)-> "<"++integer_to_list(fs2int(F,S))++">".
header(F,S)->preamble(F,S)++":"++date_().

% refactor it string:tokens -> to open brackets function
fetch_preambula(Msg)->  %io:format("~p:~p ~p ~n",[?MODULE,?LINE,Msg]),
                        try string:tokens(Msg,"<>") of 
                           [H|_] ->  list_to_integer(H)
                         catch
                         
                          Err:Reason -> ?CATCH_HANDLER_FA(fetch_preambula,Msg) 
                     
                          %Err:Reason->io:format("~p:~p ~p ~p ~n",[?MODULE,?LINE,Err,Reason])  
                        end.
                        
play_file(Name)->play_file(Name,{127,0,0,1},514).
play_file(Name,IP,Port)->
 {ok,L}=file:read_file(Name),
 lists:map(fun(A)-> ask(IP,Port,A) end, string:tokens(binary_to_list(L),"\n")).

play_file()->play_file("tsyslog.log"). 
                         