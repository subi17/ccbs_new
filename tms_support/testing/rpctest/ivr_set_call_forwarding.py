import re
import sys
from ivr_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python ivr_set_call_forwarding.py 633000431"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

q=s.ivr.set_call_forwarding(var1,{'cfb':'VM','cfnrc':'VM','cfnry':'VM'})
print_results(q)
