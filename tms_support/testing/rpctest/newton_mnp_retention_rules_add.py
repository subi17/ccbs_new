import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CLIType"
   print "Example: python newton_mnp_retention_rules_add.py CONT"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.mnp_retention_rules.add({'username':'vikasagr',
                                    'paytype':'CONT'})
print_results(p)
