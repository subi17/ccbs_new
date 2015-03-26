import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OrderId"
   print "Example: python newton_orders__set.py 10086360"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.orders__set(var1,'jannetou',{},{},{})
print_results(p)
