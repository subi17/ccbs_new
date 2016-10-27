import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_search_mobsub.py 722796855 msisdn"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   if len(sys.argv) < 3: var2 = "msisdn"
   else:
      var2 = sys.argv[2]

p=s.newton.search_mobsub(var1,10,0,var2)
print_results(p)
