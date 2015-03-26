import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OfferItemId"
   print "Example: python newton_offer_items__set.p 260163"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.offer_items__set(var1, {"username":'vikasagr'})
print_results(p)
