import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_invoices__einvoice_query.py 622689226"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.invoices__einvoice_query(
                              {"period":201206,
                              "dni":'74467657M',
                              "msisdn":var1,
                              "hash":'ce096646e25baf5e2bc875fb719371256e501b0a'})
print_results(p)
