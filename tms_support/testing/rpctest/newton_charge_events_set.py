import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: FeeModel"
   print "Example: python newton_charge_events_set.py ERKKI"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = date_time_builder('20131231')

p=s.newton.charge_events.set(var1,{'username':'vikasagr',
                             'name':'ERKKI',
                             'amount':4.0,
                             'valid_to':var2})
print_results(p)
