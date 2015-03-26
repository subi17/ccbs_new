import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_reserve_number.py 622689226"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.reserve_number(var1,False)
print_results(p)
