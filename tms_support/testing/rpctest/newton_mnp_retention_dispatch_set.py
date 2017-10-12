import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Retention platform"
   print "Example: python newton_mnp_retention_dispatch_set.py 15 40.0"
   sys.exit()

if len(sys.argv) < 3: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]

p=s.newton.mnp_retention_dispatch.set('yoigo',
                                      'vikasagr',
                                     [{'id':var1,'percentage':float(var2]}])
print_results(p)
