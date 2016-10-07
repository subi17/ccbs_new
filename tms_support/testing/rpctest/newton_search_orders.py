import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_search_orders.py 722796855"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.search_orders(var1,'msisdn','NIF',3)
print p
# print_results(p)
# If you need more beautiful output of results, comment print p and uncomment above print_results(p). Default is better for reusage of Robot Frmwrk etc.

