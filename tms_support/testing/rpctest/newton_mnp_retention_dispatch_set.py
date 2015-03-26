import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Retention platform"
   print "Example: python newton_mnp_retention_dispatch_set.py 3"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.mnp_retention_dispatch.set('vikasagr',
                                     [{'id':var1,'percentage':34.00}])
print_results(p)
