import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Active"
   print "Example: python newton_offers_list.py True"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   pst1 = sys.argv[1]
   if pst1.lower() == 'true':
      var1 = True
   else:
      var1 = False
p=s.newton.offers.list({'active':var1})
print_results(p)
