import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: BillCode"
   print "Example: python newton_billing_items_set.py 01177100"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.billing_items.set(var1,
                {'username':'vikasagr',
                 'ui_order':0,
                 'name':'iPhone',
                 'active':True
                })

print_results(p)
