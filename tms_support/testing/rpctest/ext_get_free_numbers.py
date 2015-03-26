import re
import sys
from ivr_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python ext_get_free_numbers.py 633000425"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

q=s.ext.get_free_numbers('VIP',var1)
print_results(q)
