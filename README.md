# ERLANG OTP SYSLOG DAEMON 

Scalable syslog daemon for Erlang OTP cluster with routing at syslog protocol level 
by client IP, and syslogs Application and severity ID parts of tagged syslog message,
optionnaly message might be routed by it's text content.
Also it can accumulate and handle statistics of working listeners, queues
and workers.

## ARCHITECTURE

Workers live inside separate VMs, named like 'workers_1@host.com' or for common case 'worker*'.

Syslog listenters is working inside VMs like 'syslog*@host.com'.

Syslog_queues is proxy proceses between listener and workers pool.
The main task of syslog_queue prosess is to caching of incoming messages at
the moment when workers is restarting.
 
Listener redirects all incoming messages to syslog_queue pool.
Syslog_queue do distribution for messages to workers pool.  

Mapping pid <=> worker name is made by 'global' erlang module.
Then main listener restarts workers keep working. 
So, functionality of module 'global' is using as persistant storage and naming service
for workers Pids and process names, it may be changed to another in future.

Syslog_queue can start or restart workers on 'workers*' node if needed.
Amount of workers may be relative large like 512.
Supervisor controls only listener and syslog_queue.
Worker process if died will be restarted by syslog_queue by reason of unaccesibility.

Workers nodes must started first, and then must started listeners node.
If listener starts it connects to workers or just restart workers processes.
Listener may be not alone, but inside separated erl. 

Choosing name of workers for creating it new new process is random.
It globally named as worker_$N , where $N is globally unique worker number.

All needed for configuration is stored in common.hrl file.
For real using well be more safe to change port number to some more high (like 10514) and
made port redirection 514 -> 10514 for avoid of working listeners by root permissions.

All incoming messages inside workers will be show as looger:critical/1 call by reason of demo mode and
currently do not have and backends or plugins for handling it.

## DEPENDENICIES

None, just pure Erlang OTP. 
   
COMPILE

$make || erl -make
$chmod +x *.sh

## RUN 

1) For first run workers nodes inside separated terminal sessions.

$workers_1.sh

$workers_1.sh

2) Run syslog daemon inside separated terminal too.
   for working on syslog port 514 will needs root's permisions.
    
$sudo syslogd_1.sh 

## TESTS

Inside some active erl session type:

1>sysloger:test(). Test for first single message testing.
2>sysloger:tests(). Test for set of mesages and cheching they routing.

CHANGE SYSLOG ROUTING RULES

For changing routing edit syslog_worker:route(Ip,App,Sev) function and recompile this module

## TODO

0) Use for queues global naming and separated nodes
1) Different backends for store events in REST Databases;
2) Changing syslog routing rules on fly;
3) Changing logging level on fly 
4) Chande working code on fly
5) Workers on some set of nodes (It allready done)

## HASH TAGS

#erlang_syslog_demo
#Dmitrii_Podkorytov 
#erlang 
#erlang_otp 
#erlang_cluster 
#erlang_example 
#erlang_cluster_udp_example 
#erlang_syslog 
#syslog 
#syslog_cluster
#erlang_logging
#learn_erlang 


## GET WORKING METRICS

Gor getting statistics data collections use function 'syslog_stat:info().' or more complex
'syslog_stat:all().'

syslog_stat:info() will return result like:

#{msacc =>
      [{'syslog@de.de',
           [#{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,
                    other => 2293,port => 4266,sleep => 108939738},
              id => 0,type => async},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 5479,check_io => 5603,emulator => 0,gc => 0,
                    other => 2977,port => 0,sleep => 108931932},
              id => 1,type => aux},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946288},
              id => 1,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946277},
              id => 2,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946269},
              id => 3,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946261},
              id => 4,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946256},
              id => 1,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946251},
              id => 2,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946246},
              id => 3,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946242},
              id => 4,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946237},
              id => 5,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946232},
              id => 6,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946228},
              id => 7,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 535809,gc => 0,
                    other => 19483,port => 0,sleep => 108390930},
              id => 8,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 66517,gc => 0,
                    other => 19498,port => 0,sleep => 108860202},
              id => 9,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 108946207},
              id => 10,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 1997,emulator => 0,gc => 0,
                    other => 0,port => 0,sleep => 108944196},
              id => 0,type => poll},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 16131,check_io => 9026,emulator => 173936,
                    gc => 10168,other => 102979,port => 51254,
                    sleep => 108582535},
              id => 1,type => scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 353,check_io => 126,emulator => 0,gc => 0,
                    other => 3893,port => 0,sleep => 108941645},
              id => 2,type => scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 2719,check_io => 912,emulator => 19749,gc => 2086,
                    other => 29043,port => 3092,sleep => 108888503},
              id => 3,type => scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 13758,check_io => 5917,emulator => 239101,
                    gc => 15703,other => 104183,port => 45612,
                    sleep => 108521599},
              id => 4,type => scheduler}]},
       {'workers_1@de.de',
           [#{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227819},
              id => 0,type => async},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 2404,check_io => 2617,emulator => 0,gc => 0,
                    other => 1704,port => 0,sleep => 135220942},
              id => 1,type => aux},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227802},
              id => 1,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227810},
              id => 2,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227793},
              id => 3,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227781},
              id => 4,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227772},
              id => 1,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227762},
              id => 2,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227752},
              id => 3,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227735},
              id => 4,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 403271,gc => 0,
                    other => 25104,port => 0,sleep => 134799366},
              id => 5,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227726},
              id => 6,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227692},
              id => 7,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227716},
              id => 8,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227710},
              id => 9,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135227703},
              id => 10,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 779,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 135226891},
              id => 0,type => poll},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 9169,check_io => 6066,emulator => 263006,
                    gc => 12698,other => 36360,port => 47199,
                    sleep => 134853024},
              id => 1,type => scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 266,check_io => 25,emulator => 0,gc => 0,
                    other => 4953,port => 0,sleep => 135222418},
              id => 2,type => scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 223,check_io => 85,emulator => 0,gc => 0,
                    other => 6477,port => 0,sleep => 135220866},
              id => 3,type => scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 224,check_io => 238,emulator => 0,gc => 0,
                    other => 3818,port => 0,sleep => 135223343},
              id => 4,type => scheduler}]},
       {'workers_2@de.de',
           [#{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613700},
              id => 0,type => async},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 3019,check_io => 3305,emulator => 0,gc => 0,
                    other => 2179,port => 0,sleep => 131604677},
              id => 1,type => aux},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613690},
              id => 1,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613680},
              id => 2,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613670},
              id => 3,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613655},
              id => 4,type => dirty_cpu_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613645},
              id => 1,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613633},
              id => 2,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613624},
              id => 3,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613614},
              id => 4,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613602},
              id => 5,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 8516,gc => 0,
                    other => 26046,port => 0,sleep => 131579026},
              id => 6,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613577},
              id => 7,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613535},
              id => 8,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613571},
              id => 9,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 0,emulator => 0,gc => 0,other => 0,
                    port => 0,sleep => 131613563},
              id => 10,type => dirty_io_scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 0,check_io => 1551,emulator => 0,gc => 0,
                    other => 0,port => 0,sleep => 131612000},
              id => 0,type => poll},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 1138,check_io => 818,emulator => 0,gc => 0,
                    other => 14536,port => 0,sleep => 131597065},
              id => 1,type => scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 415,check_io => 35,emulator => 0,gc => 0,
                    other => 10286,port => 0,sleep => 131602792},
              id => 2,type => scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 6943,check_io => 3813,emulator => 238286,
                    gc => 11481,other => 49425,port => 49276,
                    sleep => 131254180},
              id => 3,type => scheduler},
            #{'$type' => msacc_data,
              counters =>
                  #{aux => 139,check_io => 49,emulator => 0,gc => 0,
                    other => 7764,port => 0,sleep => 131605649},
              id => 4,type => scheduler}]}],
  queue =>
      #{process_info =>
            [{registered_name,syslog_queue},
             {current_function,{syslog_queue,handle_info,2}},
             {initial_call,{proc_lib,init_p,5}},
             {status,running},
             {message_queue_len,0},
             {links,[<0.88.0>]},
             {dictionary,
                 [{'$initial_call',{syslog_queue,init,1}},
                  {rand_seed,
                      {#{bits => 58,jump => #Fun<rand.21.53802439>,
                         next => #Fun<rand.18.53802439>,type => exsss,
                         uniform => #Fun<rand.19.53802439>,
                         uniform_n => #Fun<rand.20.53802439>},
                       [113032774279274176|67905508063667131]}},
                  {'$ancestors',[syslogd_sup,<0.87.0>]}]},
             {trap_exit,false},
             {error_handler,error_handler},
             {priority,normal},
             {group_leader,<0.86.0>},
             {total_heap_size,5172},
             {heap_size,2586},
             {stack_size,23},
             {reductions,18884},
             {garbage_collection,
                 [{max_heap_size,
                      #{error_logger => true,kill => true,size => 0}},
                  {min_bin_vheap_size,46422},
                  {min_heap_size,233},
                  {fullsweep_after,65535},
                  {minor_gcs,3}]},
             {suspending,[]}]},
  syslogd =>
      #{inet_info =>
            {ok,[{recv_oct,257},
                 {recv_cnt,7},
                 {recv_max,41},
                 {recv_avg,36},
                 {recv_dvi,2},
                 {send_oct,0},
                 {send_cnt,0},
                 {send_max,0},
                 {send_avg,0},
                 {send_pend,0}]},
        process_info =>
            [{registered_name,syslogd},
             {current_function,{syslogd,handle_info,2}},
             {initial_call,{proc_lib,init_p,5}},
             {status,running},
             {message_queue_len,0},
             {links,[<0.88.0>,#Port<0.39>]},
             {dictionary,
                 [{'$initial_call',{syslogd,init,1}},
                  {rand_seed,
                      {#{bits => 58,jump => #Fun<rand.21.53802439>,
                         next => #Fun<rand.18.53802439>,type => exsss,
                         uniform => #Fun<rand.19.53802439>,
                         uniform_n => #Fun<rand.20.53802439>},
                       [64579538453364162|39597052767717268]}},
                  {'$ancestors',[syslogd_sup,<0.87.0>]}]},
             {trap_exit,false},
             {error_handler,error_handler},
             {priority,normal},
             {group_leader,<0.86.0>},
             {total_heap_size,5172},
             {heap_size,2586},
             {stack_size,25},
             {reductions,15197},
             {garbage_collection,
                 [{max_heap_size,
                      #{error_logger => true,kill => true,size => 0}},
                  {min_bin_vheap_size,46422},
                  {min_heap_size,233},
                  {fullsweep_after,65535},
                  {minor_gcs,8}]},
             {suspending,[]}],
        socket => #Port<0.39>,
        state => #{args => [],listener => #Port<0.39>}},
  workers =>
      [#{info =>
             #{process_info =>
                   [{current_function,{syslog_workers,handle_info,2}},
                    {initial_call,{proc_lib,init_p,5}},
                    {status,running},
                    {message_queue_len,0},
                    {links,[]},
                    {dictionary,
                        [{'$ancestors',[<8745.102.0>]},
                         {'$initial_call',{syslog_workers,init,1}}]},
                    {trap_exit,false},
                    {error_handler,error_handler},
                    {priority,normal},
                    {group_leader,<0.86.0>},
                    {total_heap_size,759},
                    {heap_size,376},
                    {stack_size,23},
                    {reductions,445},
                    {garbage_collection,
                        [{max_heap_size,
                             #{error_logger => true,kill => true,size => 0}},
                         {min_bin_vheap_size,46422},
                         {min_heap_size,233},
                         {fullsweep_after,65535},
                         {minor_gcs,4}]},
                    {suspending,[]}]},
         no => 1,pid => <8745.103.0>,worker => worker_1},
       #{info =>
             #{process_info =>
                   [{current_function,{syslog_workers,handle_info,2}},
                    {initial_call,{proc_lib,init_p,5}},
                    {status,running},
                    {message_queue_len,0},
                    {links,[]},
                    {dictionary,
                        [{'$initial_call',{syslog_workers,init,1}},
                         {'$ancestors',[<8744.122.0>]}]},
                    {trap_exit,false},
                    {error_handler,error_handler},
                    {priority,normal},
                    {group_leader,<0.86.0>},
                    {total_heap_size,1604},
                    {heap_size,610},
                    {stack_size,23},
                    {reductions,411},
                    {garbage_collection,
                        [{max_heap_size,
                             #{error_logger => true,kill => true,size => 0}},
                         {min_bin_vheap_size,46422},
                         {min_heap_size,233},
                         {fullsweep_after,65535},
                         {minor_gcs,1}]},
                    {suspending,[]}]},
         no => 2,pid => <8744.123.0>,worker => worker_2},
       #{info =>
             #{process_info =>
                   [{current_function,{syslog_workers,handle_info,2}},
                    {initial_call,{proc_lib,init_p,5}},
                    {status,running},
                    {message_queue_len,0},
                    {links,[]},
                    {dictionary,
                        [{'$ancestors',[<8745.105.0>]},
                         {'$initial_call',{syslog_workers,init,1}}]},
                    {trap_exit,false},
                    {error_handler,error_handler},
                    {priority,normal},
                    {group_leader,<0.86.0>},
                    {total_heap_size,1220},
                    {heap_size,610},
                    {stack_size,23},
                    {reductions,260},
                    {garbage_collection,
                        [{max_heap_size,
                             #{error_logger => true,kill => true,size => 0}},
                         {min_bin_vheap_size,46422},
                         {min_heap_size,233},
                         {fullsweep_after,65535},
                         {minor_gcs,1}]},
                    {suspending,[]}]},
         no => 3,pid => <8745.106.0>,worker => worker_3},
       #{info =>
             #{process_info =>
                   [{current_function,{syslog_workers,handle_info,2}},
                    {initial_call,{proc_lib,init_p,5}},
                    {status,running},
                    {message_queue_len,0},
                    {links,[]},
                    {dictionary,
                        [{'$ancestors',[<8745.108.0>]},
                         {'$initial_call',{syslog_workers,init,1}}]},
                    {trap_exit,false},
                    {error_handler,error_handler},
                    {priority,normal},
                    {group_leader,<0.86.0>},
                    {total_heap_size,759},
                    {heap_size,376},
                    {stack_size,23},
                    {reductions,445},
                    {garbage_collection,
                        [{max_heap_size,
                             #{error_logger => true,kill => true,size => 0}},
                         {min_bin_vheap_size,46422},
                         {min_heap_size,233},
                         {fullsweep_after,65535},
                         {minor_gcs,4}]},
                    {suspending,[]}]},
         no => 4,pid => <8745.109.0>,worker => worker_4}]}
