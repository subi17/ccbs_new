import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Invalid parameters"
   print "Example parameter values: (custnum):736359"
   print "Note: in case you need to enter space 'between words' inside parameter, use (').\n"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.get_customer_details(var1)
print p
# print_results(p)
# If you need more beautiful output of results, comment print p and uncomment above print_results(p). Default is better for reusage of Robot Frmwrk etc.
