from datetime import datetime
import time, os

now = datetime.today()
os.system('/opt/local/bin/xfear_t -batch callalarm_batch tms.pf > /scratch/cron/callalarm.cron 2>&1')
