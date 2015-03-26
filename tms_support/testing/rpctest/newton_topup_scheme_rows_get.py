import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: TopupSchemeRow"
   print "Example: python newton_topup_scheme_rows_get.py 3"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.topup_scheme_rows.get([var1])
print_results(p)
