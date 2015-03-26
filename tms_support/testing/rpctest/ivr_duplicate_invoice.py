import re
import sys
from ivr_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: Invoice Number"
   print "Example: python ivr_duplicate_invoice.py 129C00073579"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

q=s.ivr.duplicate_invoice(var1)
print_results(q)
