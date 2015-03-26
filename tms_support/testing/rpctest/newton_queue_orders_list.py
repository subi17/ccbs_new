import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Order status"
   print "Example: python newton_queue_orders_list.py 6"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.queue_orders.list({'status':var1,'offset':1,'limit':10})
print_results(p)
