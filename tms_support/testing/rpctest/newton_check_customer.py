import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Person/Company ID"
   print "Example: python newton_check_customer.py 83658423V NIF"
   sys.exit()

if len(sys.argv) < 3: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]

p=s.newton.check_customer(var1,var2,False,1)
print_results(p)
