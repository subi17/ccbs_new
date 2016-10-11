import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OrderId"
   print "Example: python newton_get_order_details.py 10088467"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.get_order_details(var1)
                              
print p
# print_results(p)
# If you need more beautiful output of results, comment print p and uncomment above print_results(p). Default is better for reusage of Robot Frmwrk etc.

