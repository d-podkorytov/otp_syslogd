#!/bin/sh
echo start $0
erl -name syslog@de.de -s syslogd | tee $0.log
#-kernel_logger '[{handler,default,logger_disk_log_h,#{config => #{file => "./'$0'.log"}}}]' 
