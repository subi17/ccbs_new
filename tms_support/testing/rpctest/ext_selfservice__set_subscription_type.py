import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: MSISDN, NewSubType, ActivationStamp"
   print "Example: python ext_selfservice__set_subscription_type.py 622689226 CONTS 20140101"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]
   var3 = date_time_builder(sys.argv[3])

q=s.ext.selfservice.set_subscription_type({'transaction_id':'501',
                                           'msisdn':var1,
                                           'subscription_type_id':var2,
                                           'activation_stamp':var3,
                                           'data_bundle_id':''})
print_results(q)



