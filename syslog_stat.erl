-module(syslog_stat).
-compile(export_all).
-define(TIMEOUT,10000).
-include("common.hrl").

%info(L) when is_list(L)->
%	lists:map(fun(Pid)->{pid_to_list(Pid),erlang:process_info(Pid)} end,L);
%
%info(Pid) when is_pid(Pid)->
%	erlang:process_info(Pid);
%
%info({local,Atom})->
% info(whereis(Atom));
%
%info({global,Atom})->
% info(global:whereis_name(Atom)).
%
%info({local,Atom},Subject)->
% info(whereis(Atom),Subject);
%
%info({global,Atom},Subject)->
% info(global:whereis_name(Atom),Subject);
%
%info(Pid,Subject) when is_pid(Pid)->
%	{Subject,Value}=erlang:process_info(Pid,Subject),
%	Value. 

start()-> msacc:start().

microservices()->
        #{ syslogd => syslogd:info(),
           workers => syslog_workers:info(),
           queues  => syslog_queue:info()
         }.

% common statistics

statistics(A,B)->sys:statistics(A,B).

erts_alloc_config()-> #{ erts_alloc_config => erts_alloc_config:state()}.

os_mon_sysinfo()->
     logger:critical(#{ current_stacktrace =>  process_info(self(),current_stacktrace)}),      
    #{get_disk_info => 
                    try os_mon_sysinfo:start_link(),
                        os_mon_sysinfo:get_disk_info() of 
                    R->R 
                    catch 
                     %Err:Reason:Backtrace -> #{err => Err, reason => Reason, backtrace => process_info(self(),current_stacktrace) } 
                     Err:Reason -> ?CATCH_HANDLER({?MODULE,os_mon_info})
                    end,
     get_mem_info  => 
                   try os_mon_sysinfo:start_link(),
                       os_mon_sysinfo:get_mem_info()  of 
                   R->R 
                   catch
                    Err:Reason -> ?CATCH_HANDLER({?MODULE,os_mon_info}) 
                    %Err:Reason:Backtrace -> #{ err => Err,reason => Reason,backtrace => process_info(self(),current_stacktrace)} 
                   end
    }.

os()->
 #{type => os:type(),
   version => os:version()
  }.

applications()->
 #{applications => application:info()}.

init_info()->
    #{    
     get_arguments => init:get_arguments(),
     get_plain_arguments => init:get_plain_arguments(),
     script_id           => init:script_id(),
     get_status          => init:get_status()
    }.

erlang()->
    #{
    %{dist_get_stat,1},
     erts_alloc_config => erts_alloc_config:state(),
     node => node(),
     otp_release => ?OTP_RELEASE, % like erlang:system_info(otp_release),
     os => os(),
     msacc         => msacc(),   
     universaltime => erlang:universaltime(),
     time          => erlang:time(),
     active_tasks_all  => erlang:statistics(active_tasks_all) 
    }.

short()->#{microservices => microservices(),
           %erlang       => erlang(),
           init         => init_info(),
           applications => applications()

%         msacc             => lists:map(fun(Node)-> 
%                                            rpc:call(Node,msacc,start,[]),
%                                            {Node,rpc:call(Node,msacc,stats,[])} 
%                                        end, [node()|nodes()]),

%         erts_alloc_config => erts_alloc_config(),
%         diameter_services => diameter:services(), 
%         os_mon_sysinfo => os_mon_sysinfo() % it does not works for linux  
       }.    

msacc()-> lists:map(fun(Node)-> 
                     rpc:call(Node,msacc,start,[]),
                     {Node,rpc:call(Node,msacc,stats,[])} 
                    end, [node()|nodes()]).
                    
% set time window for msacc
msacc(Time)-> msacc:start(Time).

options()->lists:foldl(fun({FunName,0},Acc)-> [FunName|Acc];
                       (_,Acc)             ->  Acc 
                    end,
                    [],
                    ?MODULE:module_info(exports)
                   ).

help()->io:format("Run ~n1>~p:info(Opts). Opts is any set from options list ~p~n",[?MODULE,options()]).

all()->info(options()).

info(List)-> lists:foldl(fun(A,Acc) -> maps:put(A,apply(?MODULE,A,[]),Acc) end,#{},List).

% dump system information to file
system_information_to_file()->
 system_information:to_file(atom_to_list(node())++"-"++pid_to_list(self())++".information.txt").

%diameter()->diameter:services(). 

% walking in tree of processes
parent(Pid)-> {group_leader,Value}=erlang:process_info(Pid,group_leader),Value.

parents(Pid)-> H=parent(Pid),
               [H,parent(H)].

parents_info(Pid) -> lists:foldl(fun(A,Acc)-> maps:put(A,info(A),Acc) end,#{},parents(Pid)).

children(Pids) when is_list(Pids)-> lists:foldl(fun(A,Acc)-> [children(A)|Acc] end,[],Pids);
children(Pid)-> {links,Value}=erlang:process_info(Pid,links),Value.

all(Pid) when is_pid(Pid)-> L=children(Pid),
           lists:merge([L|children(L)]).
           
children_info(Pid) -> lists:foldl(fun(A,Acc)-> maps:put(A,info(A),Acc) end,#{},children(Pid)).

children_info_tree(Pid) when is_pid(Pid) -> lists:foldl(fun(A,Acc)-> maps:put(A,children_info(A),Acc) end,#{},children(Pid));
children_info_tree(X)->X.

               