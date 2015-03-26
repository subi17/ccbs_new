import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_get_top_up_history.py 10061589"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.get_top_up_history(var1,2)
                              
print_results(p)
