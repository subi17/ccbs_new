import re
import sys
from newton_init import *
from show_results import *
import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: orderid "
   print "Example: python newton_fixedline_order_update.py 70212091"
   sys.exit()
print(s)
print(sys.argv[0])
print(sys.argv[1])

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = sys.argv[2]

p=s.newton.fixedline_order_update('rarebari',var1,
                                  {
                                   'fixednumber':var2
                                  },
                                  'ChangePhoneNumber',
                                  'FNP Error')

print_results(p)




