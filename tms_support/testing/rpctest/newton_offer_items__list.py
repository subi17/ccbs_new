import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OfferItemKey"
   print "Example: python newton_offer_items__list.py P034I93W2CVE037"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.offer_items__list({'offer_id':var1})
print_results(p)
