import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OfferItemKey"
   print "Example: python newton_offers_set.py TEST_Janne"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.offers.set(var1,{"username":'jannetou',})
print_results(p)
