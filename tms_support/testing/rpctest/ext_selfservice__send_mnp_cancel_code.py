import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: OrderId"
   print "Example: python ext_selfservice__send_mnp_cancel_code.py 70004805"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

q=s.ext.selfservice.send_mnp_cancel_code('501',var1)
print_results(q)
