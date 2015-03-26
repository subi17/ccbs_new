import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: MSISDN, NewBundle"
   print "Example: python ext_selfservice__set_bundle_type.py 622689226 CONTS25"
   sys.exit()

if len(sys.argv) < 3: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]

q=s.ext.selfservice.set_bundle_type('501',var1,var2)
print_results(q)
