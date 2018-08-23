import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: orderid "
   print "Example: python newton_fixedline_order_cancellation.py 5032785"
   sys.exit()

print(sys.argv[0])
print(sys.argv[1])

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.fixedline_order_cancellation(var1,'selforde','')

print_results(p)
