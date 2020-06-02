-define(POINT,#{ module_line => {?MODULE,?LINE}, node => node(), pid => self() , local_time => calendar:local_time() , function_name => atom_to_list(?MODULE)++":"++atom_to_list(?FUNCTION_NAME)++"/"++integer_to_list(?FUNCTION_ARITY) }).

-define(POINT_ARG(MSG),#{ point =>?POINT , arg => MSG}).

-define(TRACE_POINTS_LEVEL,debug).

-define(CATCH_HANDLER(Info),
              logger:critical(#{ current_stacktrace =>  process_info(self(),current_stacktrace), info => Info, point => ?POINT_ARG("catch inside try")}),      
              #{err => Err, reason => Reason, backtrace => process_info(self(),current_stacktrace), info => Info, point => ?POINT_ARG("catch inside try")}). 

-define(CATCH_HANDLER_FA(F,A),?CATCH_HANDLER({?MODULE,F,A})).

-define(WORKERS_NUM,4).
-define(WORKERS_NODES,['workers_1@de.de','workers_2@de.de']).
                    