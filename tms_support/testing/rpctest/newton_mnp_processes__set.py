import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MNPSeq"
   print "Example: python newton_mnp_processes__set.py 22231"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.mnp_processes.set(var1,
                            {'operation_id':'cancel',
                             'username':'vikasagr',
                             'reason_code':'',
                             'memo':
                             {
                             'title':'otsikko',
                              'content':'something inside',
                              'source':'VISTA MN'
                             }
                            })
print_results(p)
