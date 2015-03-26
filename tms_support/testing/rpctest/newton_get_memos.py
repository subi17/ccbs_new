import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CustNum"
   print "Example: python newton_get_memos.py 3046310"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.get_memos(var1,'mobsub',10040400,5)
                              
print_results(p)
