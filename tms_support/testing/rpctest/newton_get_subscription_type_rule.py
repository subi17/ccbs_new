import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Example: python newton_get_subscription_type_rule.py 10055111 TARJ7 ''"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = sys.argv[2]
   var3 = sys.argv[3]

p=s.newton.get_subscription_type_rule(var1,var2,var3)
                              
print_results(p)
