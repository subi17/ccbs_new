import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN, prepaid balance, balance type"
   print "Example: python newton_add_balance.py 123456789 10.00 TOP"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = sys.argv[1]
   var2 = float(sys.argv[2])
   var3 = sys.argv[3]

p=s.newton.add_balance(var1,var2,var3)
print_results(p)
