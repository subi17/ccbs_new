import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Periodical Term"
   print "Example: python newton_periodical_contracts_get.py YOIGOYOIGO"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.periodical_contracts.get([var1])
print_results(p)
