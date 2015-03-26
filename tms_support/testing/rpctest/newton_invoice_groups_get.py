import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: InvTargetGroup"
   print "Example: python newton_invoice_groups_get.py 190063"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.invoice_groups.get([var1])
print_results(p)
