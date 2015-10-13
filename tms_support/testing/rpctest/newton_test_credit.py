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

p=s.newton.test_credit({'main_invoice':var1,
                            'sub_invoices':[{'sub_invoice':1,
                             'inv_rows':[{'inv_row':714215389,'amount':9.09},
                             {'inv_row':714215390,'amount':4.96}
                             ]}], 
                             'username':'vikasagr',
                             'reason_category':'',
                             'reason':'3010',
                             'remark':'Correct'})
print_results(p)
