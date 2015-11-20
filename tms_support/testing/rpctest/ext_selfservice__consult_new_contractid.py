import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: CustId"
   print "Example: python ext_selfservice__consult_new_contractid.py ZJ848210"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

q=s.ext.selfservice.consult_new_contractid('501','NIF',var1,'EMAIL')
print_results(q)
