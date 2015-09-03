import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN, Segmentation offer"
   print "Example: python newton_change_segmentation.py 123456789 OFA"
   sys.exit()

if len(sys.argv) < 3: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]

p=s.newton.change_segmentation(var1,var2)
print_results(p)
