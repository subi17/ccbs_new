import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CustNum"
   print "Example: python newton_invoices__get.py 78592"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = date_time_builder('20130601')

p=s.newton.invoices.list({'customer_id':var1,
                          'invoice_date_start':var2})
print_results(p)
