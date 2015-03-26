import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: BillGroup"
   print "Example: python newton_billing_items_list.py 6"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.billing_items.list({'billing_group':var1})

print_results(p)
