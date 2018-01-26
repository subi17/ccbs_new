import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CLIType"
   print "Example: python newton_bundle_items__list.py CONTF"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]

p=s.newton.bundle_items__list({"brand": var1, "subscription_type_id": '{0}|{1}'.format(var2,var1)})
print_results(p)
