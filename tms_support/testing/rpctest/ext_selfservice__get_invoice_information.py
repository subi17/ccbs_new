import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Invalid parameters"
   print "Example parameter values: (transid):501 (CLI):622648607 (billPeriod):20130722"
   print "Note: in case you need to enter space 'between words' inside parameter, use (').\n"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]
   var3 = int(sys.argv[3])

q=s.ext.selfservice.get_invoice_information(var1,var2,var3)
print_results(q)
