import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python tms_topup_events.py 722000184"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = date_time_builder('20130927')

p=s.tms.topup_event({'type':'recharge',
                       'received_at':var2,
                       'date':var2,
                       'entity_index':03,
                       'reference':'',
                       'num_oper':'',
                       'local_code':'',
                       'subscriber_number':var1,
                       'origin_entity':'',
                       'postal_code':'',
                       'netplus_result_code':14588499,
                       'amount_with_tax':3,
                       'amount_without_tax':2,
                       'tax_percent':30,
                       'tax_zone':1
                      })
print_results(p)
