import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_invoice_groups_add.py 10061498"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.invoice_groups.add({'customer_id':4457898,
                               'username':'vikasagr',
                               'subscriptions':[var1]})
print_results(p)
