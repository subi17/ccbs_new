import re
import sys
from ivr_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
def instruction():
   print "Missing parameter: MsSeq Upsell_ID"
   print "Example: python ext_upsell_bundles_set.py 10051640 DATA6_UPSELL"
   sys.exit()

if len(sys.argv) < 3: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = sys.argv[2]

q=s.ext.upsell_bundles_set(var1,var2)
print_results(q)
