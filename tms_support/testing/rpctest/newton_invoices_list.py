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
# This is now hard coded value, but can be easily changed parameter
var3 = 'yoigo'

p=s.newton.invoices.list({'brand':var3,'customer_id':var1,
                          'invoice_date_start':var2})

# print for robotframework
print p

# print for human output
#print_results(p)
