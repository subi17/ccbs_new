import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: OrderId MNP_Request_ID (SMS_cancel_code)"
   print "Example: python ext_selfservice__cancel_mnp_order.py 70004575 005D0006171725025135103"
   sys.exit()

if len(sys.argv) < 3: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = sys.argv[2]
   var3 = sys.argv[3]

q=s.ext.selfservice.cancel_mnp_order('501',var1,var2,var3)
print_results(q)
