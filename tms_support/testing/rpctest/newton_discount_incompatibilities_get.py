import re
import sys
import os
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: brand"
   print "Example: python {0} yoigo".format(os.path.basename(__file__))
   sys.exit(1)

if len(sys.argv) < 2:
   instruction()
else:
   var1 = sys.argv[1]

l=s.newton.discount_incompatibilities.list({"brand": var1})

g=s.newton.discount_incompatibilities.get(l)

print_results(g)
