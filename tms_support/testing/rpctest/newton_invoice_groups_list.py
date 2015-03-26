import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CustNum"
   print "Example: python newton_invoice_groups_list.py 4457909"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.invoice_groups.list({'customer_id':var1,
                              'limit':3,
                              'offset':2
                              })
print_results(p)
