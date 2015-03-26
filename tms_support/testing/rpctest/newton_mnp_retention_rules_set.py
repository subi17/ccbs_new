import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: RetentionRuleID"
   print "Example: python newton_mnp_retention_rules_set.py 22"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.mnp_retention_rules.set({'username':'vikasagr',
                                    'id':var1})
print_results(p)
