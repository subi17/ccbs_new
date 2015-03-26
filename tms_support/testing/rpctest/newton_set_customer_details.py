import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Person/Customer ID"
   print "Example: python newton_set_customer_details.py 3717582"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.set_customer_details(var1,'vikasagr',True,{},{'title':'Mr'})
print_results(p)
