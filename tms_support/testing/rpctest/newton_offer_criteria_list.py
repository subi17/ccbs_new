import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OfferCriteriaID"
   print "Example: python newton_offer_criteria_list.py 140777"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.offer_criteria.list({'offer_id':var1,'criteria_type':'CLIType'})
print_results(p)
