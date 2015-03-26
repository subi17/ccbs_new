import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: OrderId"
   print "Example: python ext_selfservice__cancel_mnp_order.py 10087533"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

q=s.ext.selfservice.cancel_mnp_order('501',var1,'005D0099635887081726601')
print_results(q)
