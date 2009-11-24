#!/bin/bash
export LOG_PATH=$PWD/pressure_monitor.log
export SASL_LOG_PATH=$PWD/pressure_monitor_sasl.log
exec erl -noinput -detached  -kernel error_logger \{file,\"$LOG_PATH\"\} \
                             -sasl sasl_error_logger \{file,\"$SASL_LOG_PATH\"\} \
                             -boot start_sasl \
                             -s pressure_monitor_app \
                             +K true +P 99999 +h 99999