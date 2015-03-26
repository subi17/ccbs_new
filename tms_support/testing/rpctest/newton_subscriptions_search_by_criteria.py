import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CLIType"
   print "Example: python newton_subscriptions_search_by_criteria.py CONTF"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.subscriptions.search_by_criteria({'subscription_type':var1,
                                             'subscription_bundle_id':'CONTSF10'},
                                              0,
                                              10)
print_results(p)
