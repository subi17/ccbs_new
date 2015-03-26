import re
import sys
from ivr_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
def instruction():
   print "Missing parameter: MSISDN, NewBundle, ActivationStamp"
   print "Example: python ext__set_bundle_type.py 622689226 CONTS25 20140101"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = sys.argv[2]
   var3 = date_time_builder(sys.argv[3])


q=s.ext.set_bundle_type(var1,var2,False,var3)
print_results(q)
