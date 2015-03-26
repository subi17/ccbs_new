import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: FeeModel"
   print "Example: python newton_charge_events_add.py SEPPO"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = date_time_builder('20130701')

p=s.newton.charge_events.add({'username':'vikasagr',
                              'id':var1,
                              'amount':5.0,
                              'billing_item_id':'YOIGOYOIGO',
                              'paytype':'postpaid',
                              'valid_from':var2})
print_results(p)
