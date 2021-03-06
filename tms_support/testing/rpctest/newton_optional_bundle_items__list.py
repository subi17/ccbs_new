import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CLIType"
   print "Example: python newton_optional_bundle_items__list.py CONT7"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.optional_bundle_items__list({'subscription_type_id':var1})
print_results(p)
