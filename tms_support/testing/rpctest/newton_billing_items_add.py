import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: BillCode"
   print "Example: python newton_billing_items_add.py 01177100"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.billing_items.add(
                {'username':'vikasagr',
                 'id':var1,
                 'name':'Janne',
                 'billing_group':'7'
                })

print_results(p)
