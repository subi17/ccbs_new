import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OrderId"
   print "Example: python newton_imeis_release.py 10048020 "
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.imeis.release([{'imei':'355310040000006',
                           'imei_status':0,
                           'order_id':var1
                           }])
print_results(p)
