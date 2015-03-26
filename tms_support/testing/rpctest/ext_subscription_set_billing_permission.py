import re
import sys
from ivr_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python ext_subscription_set_billing_permission.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

q=s.ext.subscription.set_billing_permission(var1,1)
print_results(q)
