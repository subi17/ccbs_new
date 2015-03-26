import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: RequestType"
   print "Example: python newton_requests_search.py 83"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.requests.search({'type':var1,'status':19},1,3)
print_results(p)
