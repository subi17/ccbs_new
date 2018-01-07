import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OrderId"
   print "Example: python newton_get_msisdn.py 70143800"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.get_msisdn('yoigo',var1)
print_results(p)
