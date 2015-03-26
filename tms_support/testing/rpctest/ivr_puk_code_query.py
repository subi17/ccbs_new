import re
import sys
from ivr_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python ivr_puk_code_query.py 633000431"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

q=s.ivr.puk_code_query(var1)
print_results(q)
