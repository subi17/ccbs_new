from datetime import datetime
import time, os, sys

loop = 0
node = '' 

if len(sys.argv) == 2:
   node = sys.argv[1]

if node != '1' and node != '2' and node != '3' and node != '4':
   sys.exit("Must give node 1 or 2 or 3 or 4")

while True:
   loop = loop + 1
   now = datetime.today()
   print loop, now.strftime('%m.%d.%Y %H:%M:%S')
   os.system('/opt/local/bin/xfear -batch callalarm_batch tms.pf ' + node + ' > /scratch/cron/callalarm_' + node + '.cron 2>&1')
   time.sleep(5)
   if not os.path.exists('/apps/tms/lock/callalarm_batch_' + node + '.lock'):
      break
