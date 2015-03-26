import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Search pattern, can be '' too"
   print "Example: python newton_get_free_numbers.py 663300"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.get_free_numbers(var1)
                              
print_results(p)
