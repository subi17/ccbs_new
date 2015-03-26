import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: InvTargetGroup"
   print "Example: python newton_invoice_groups_set.py 10061583"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.invoice_groups.set(180063,
                             {'username':'vikasagr',
                              'active':False,
                              'subscriptions':[var1]
                             })
print_results(p)
