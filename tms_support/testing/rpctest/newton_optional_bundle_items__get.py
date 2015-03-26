import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: BundleItemId"
   print "Example: python newton_optional_bundle_items__get.py CONTS30"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.optional_bundle_items__get([var1])
print_results(p)
