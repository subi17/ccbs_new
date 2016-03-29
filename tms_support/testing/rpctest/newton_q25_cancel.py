import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Action (remove or cancel)"
   print "Example: python newton_q25_cancel.py cancel 9500140 123456789"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = sys.argv[1]
   var2 = int(sys.argv[2])
   var3 = int(sys.argv[3])

p=s.newton.q25_cancel({'q25_struct':
                       {'username':'tester',
                       'msseq':var2,
                       'per_contract_id':var3,
                       'action':var1},
                       {'title':'test memo',
                       'content':var1}})
print_results(p)
