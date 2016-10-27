import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)

def instruction():
   print "Missing parameter: OrderID"
   print "Example: python newton_fusion_orders__set.py 10088477"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.fusion_orders.set({'username':'python-script',
                              'order_id': var1,
                              'fusion_order_status':'CAN',
                              })
print_results(p)
