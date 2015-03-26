from datetime import datetime
import time, os

loop = 0

while True:
   loop = loop + 1
   now = datetime.today()
   print loop, now.strftime('%m.%d.%Y %H:%M:%S')
   os.system('/opt/local/bin/xfear -batch reqrun_batch tms.pf > /scratch/cron/reqrun.cron 2>&1')
   time.sleep(5)
   if not os.path.exists('/apps/tms/lock/reqrun_batch.lock'):
      break
