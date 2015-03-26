import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_set_subscription_type.py 622689226"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = date_time_builder('20130801')

p=s.newton.set_subscription_type(
                              {'msisdn':var1,
                              'username':'vikasagr',
                              'subscription_type_id':'CONTF',
                              'data_bundle_id':'CONTF10',
                              'activation_stamp':var2,
                              'charge':0.0,
                              'charge_limit':0.0,
                              }
                              )
print_results(p)

