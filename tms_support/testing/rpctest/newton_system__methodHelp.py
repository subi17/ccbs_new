import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Invalid parameters"
   sys.exit()

if len(sys.argv) < 1: instruction()
else:
   method_name = sys.argv[1]

p=s.system.methodHelp(method_name)
print_results(p)
