import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq per contrId new_amount"
   print "Example: python newton_change_payterm.py 10055111 12345 80"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = int(sys.argv[2])
   var3 = float(sys.argv[3])

p=s.newton.q25_change({
                        'q25_struct':{'msseq':var1,
                               'per_contract_id':var2,
                               'new_amount':var3,
                               'username':'kaaikas'},
                        'memo_struct':{'title':'testing',
                               'content':'changing q25 contract'}
                             } )
print_results(p)
