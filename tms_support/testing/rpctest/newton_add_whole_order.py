import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_add_whole_order.py 622689226"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.add_whole_order(
                {'channel':'pos',
                 'cli':var1,
                 'contractid':'37',
                 'orderer_ip':'172.20.48.67',
                 'subscription_type':'CONTF',
                 'order_inspection_result':'',
                 'number_type':'renewal'},
                {'city':'Madrid',
                 'street':'100011',
                 'zip':'28600',
                 'person_id':'old'},
                {},
                {},
                {})

print_results(p)
