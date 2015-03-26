import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Category"
   print "Example: python newton_customer_categories_set.py 10"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.customer_categories.set(var1,
                                 {'limit':5,
                                  'username':'vikasagr',
                                  'activationlimit':5})
                              
print_results(p)
