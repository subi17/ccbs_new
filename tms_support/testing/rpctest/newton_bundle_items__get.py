import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: BundleItem"
   print "Example: python newton_bundle_items__get.py CONTS30"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = 'CONTF10|yoigo'
   var2 = 'CONTF8|yoigo'

p=s.newton.optional_bundle_items__get([var1,var2])
print_results(p)
