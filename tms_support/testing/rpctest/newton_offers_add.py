import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OfferItemKey"
   print "Example: python newton_offers_add.py TEST_Janne"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = date_time_builder('20130926')

p=s.newton.offers.add(
                    {
                     "id":var1,
                     "valid_from":var2,
                     "username":'jannetou',
                     "description":'Testing purposes only',
                     "priority":10,
                     "active":True
                     }
                    )
print_results(p)
