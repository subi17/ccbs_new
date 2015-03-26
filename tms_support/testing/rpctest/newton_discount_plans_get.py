import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: DPRuleId"
   print "Example: python newton_discount_plans_get.py BONO8DISC"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.discount_plans.get([var1])
                              
print_results(p)
