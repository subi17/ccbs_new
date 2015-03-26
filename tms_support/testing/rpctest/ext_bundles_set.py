import re
import sys
from ivr_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python ext_bundles_set.py 10051634"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

q=s.ext.bundles_set(var1,'BONO_VOIP','on')
print_results(q)
