import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OfferItemKey"
   print "Example: python newton_offer_items__add.py P034I93W2CVE037"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = date_time_builder('20130131')

p=s.newton.offer_items__add(
                           {
                            "offer_id":var1,
                            "valid_from":var2,
                            "username":'vikasagr',
                            "display_in_ui":True,
                            "display_on_invoice":True,
                            "item_id":'MDUB',
                            "item_type":'BundleITem',
                            "vat_included":False
                            }
                           )
print_results(p)
