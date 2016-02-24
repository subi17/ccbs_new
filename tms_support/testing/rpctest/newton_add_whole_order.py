import re
import sys
from newton_init import *
from show_results import *


# Definition
# PArameters CLI CHANNEL PAYMENT_METHOD PAYER_ID PAYMENT_REFERENCE CONTRACTID
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_add_whole_order.py 622689226 newton paypal 555 111 222"
   sys.exit()

if len(sys.argv) < 7: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]
   var3 = sys.argv[3]
   var4 = sys.argv[4]
   var5 = sys.argv[5]
   var6 = sys.argv[6]

p=s.newton.add_whole_order(
                 {'order_data':{'channel':var2,
                 'cli':var1,
                 'payer_id':var4,
                 'payment_reference':var5,
                 'contractid':var6,
                 'orderer_ip':'172.20.48.67',
                 'subscription_type':'CONT',
                 'order_inspection_result':'',
                 'number_type':'new',
                 'reseller':'1',
                 'subscription_bundle':'',
                 'sim_type':'nano',
                 'delivery_type':1,
                 'payment_method':var3},
                 'customer_data':{'city':'Madrid',
                 'street':'100011',
                 'zip':'28600',
                 'person_id':'old',
                 'customer_data_retrieved':bool(1)}})

print_results(p)
