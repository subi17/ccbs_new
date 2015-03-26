import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CustNum"
   print "Example: python newton_customers_set_counter_limit.py 4457898"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.customers.set_counter_limit(var1,'vikasagr',1,1,50.0)
                              
print_results(p)
