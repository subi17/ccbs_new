import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CustNum"
   print "Example: python newton_get_order_customer_details.py 10088477"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.get_order_customer_details(var1)
print_results(p)
