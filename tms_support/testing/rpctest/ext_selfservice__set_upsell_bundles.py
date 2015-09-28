import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
#s = xmlrpclib.ServerProxy(address)

def instruction():
   print "Missing parameter: MSISDN BUNDLE SOURCE"
   print "Example: python ext_selfservice__set_upsell_bundles.py 622689226 DATA9_UPSELL 501"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]
   var3 = sys.argv[3]

q=s.ext.selfservice.set_upsell_bundles(var3,var1,var2)
print_results(q)


