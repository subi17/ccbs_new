import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OrderId"
   print "Example: python newton_set_order_delivery_address.py 10088417"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.set_order_delivery_address(var1,'jannetou',
                                      {'street':'WindyStreet 45',
                                       'zip':'11500',
                                       'Region':'24',
                                       'city':'Madrid'
                                       })
print_results(p)
