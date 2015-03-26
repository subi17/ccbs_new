import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: InvNum"
   print "Example: python newton_credit_notes_set.py 10846900"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.credit_notes.set({'main_invoice':var1,
                             'sub_invoices':[],
                             'username':'vikasagr',
                             'reason_category':'Accounts',
                             'reason':'AccType',
                             'remark':'Correct'})
print_results(p)
