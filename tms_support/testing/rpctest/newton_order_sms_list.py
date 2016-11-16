import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OrderID"
   print "Example: python newton_order_sms_list.py 70048186"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.order_sms.list({"order_id":var1,"limit":30,"offset":0})
print_results(p)

