import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python tms_get_yoigoyoigo_balanace_sms.py 722000184"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.tms.get_yoigoyoigo_balance_sms(var1)
print_results(p)
