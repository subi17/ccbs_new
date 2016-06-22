import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CLIType invoicegrp language"
   print "Example: python newton_subscriptions_search_by_criteria.py CONTF VAT1 1"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]
   var3 = sys.argv[3]

p=s.newton.subscriptions.search_by_criteria({'subscription_type':var1,
                                             'invoice_group':var2,
                                             'language':var3,
                                             'subscription_bundle_id':'CONTSF10'},
                                              0,
                                              10)
print_results(p)
