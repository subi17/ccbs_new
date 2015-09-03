import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python ext_selfservice__set_bundles.py 622689226 501 MDUB off"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]
   var3 = sys.argv[3]
   var4 = sys.argv[4]

q=s.ext.selfservice.set_bundles(var2,var1,var3,var4)
print_results(q)
