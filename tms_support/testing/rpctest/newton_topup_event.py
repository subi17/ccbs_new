import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python tms_topup_event.py 600004513"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = date_time_builder('20131029')




p=s.tms.topup_event({
                    'subscriber_number':var1,
                    'amount_with_tax':1100,
                    'amount_without_tax':1000,
                    'local_code':"00000000",
                    'type':'recharge',
                    'received_at':var2,
                    'date':var2,
                    'entity_index':03,
                    'reference':'123- 732',
                    'num_oper':'xyz1',
                    'tax_zone':1,
                    'tax_percent':10,
                    'origin_entity':'foo',
                    'postal_code':"28800",
                    'netplus_result_code':140
                    })
print_results(p)
