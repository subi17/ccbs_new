import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsRequestID"
   print "Example: python newton_requests_get.py 2139"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.requests.get([var1])
print_results(p)
