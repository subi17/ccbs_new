#!/bin/bash

new_cdr_pf=/tmp/temp_new_cdr.pf
PROPATH=/apps/xfera/tms
export PROPATH
export display_banner=no

mpro -b -p Syst/create_cdrpf.p -db /db1/common/common -param ${new_cdr_pf}

if [ -f "${new_cdr_pf}" ];
then
   /opt/local/bin/xfear -batch Rate/rerate_request_start ${new_cdr_pf} 2>&1 | /opt/local/bin/parse_mpro_batch.py rerate_new_cdrdb >> /scratch/cron/batch.log
fi
