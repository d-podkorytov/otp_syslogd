% service name <=> pid mapping

-module(syslog_ns).
-compile(export_all).
-include("common.hrl").

% set mapping Name <=> Pid
% if mapping exists kill old Pid and register new for given name
set(Name,Pid) when is_pid(Pid) ->
 case pid(Name) of    
  undefined -> R=global:register_name(Name,Pid),
               logger:debug(#{point => ?POINT,name => Name, pid => Pid, result => R, reason => "global:register name" }),
               Pid;
  
  OldPid       -> %global:re_register_name(Name,Pid),
                  erlang:exit(Pid,normal),
                  OldPid
                  %global:register_name(Name,Pid)
 end.

% set maping for composed name like name_10
set(Name,N,Pid)-> 
 set(compose_name(Name,N),Pid).

% get pid by name
pid(Name)   -> global:whereis_name(Name).
pid(Name,N) -> global:whereis_name(compose_name(Name,N)).
pids()-> lists:map(fun(N)-> {N,pid(worker,N)} end, lists:seq(1,?WORKERS_NUM)).

% compose name like name_22
compose_name(Atom,N)-> 
 list_to_atom(atom_to_list(Atom)++"_"++integer_to_list(N)).

% TESTS
loop()->
 receive
  Msg -> logger:critical(#{msg => Msg, point => ?POINT , reason => "inside_loop" })
 end,
 loop().

test()->
 set(?MODULE,spawn(?MODULE,loop,[])),
 pid(?MODULE)!{self(),test}.

cluster()->[node()|nodes()].

get_nodes_by_role(Role) -> lists:foldl(fun(_,[])   -> [];
                                          (A,Acc)  -> logger:debug(#{acc => Acc, starts => starts(atom_to_list(Acc),Role) }),
                                                      case starts(atom_to_list(A),Role) of
                                                        true -> [A|Acc];
                                                        _    -> Acc
                                                      end 
                                   end, [], cluster()).

starts(Name,Starts)-> 
 logger:debug(#{point => ?POINT,name => Name, ext => Starts}),
 string:str(Name,Starts) == 1.
   