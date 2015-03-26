import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Orderchannel"
   print "Example: python newton_set_order_cancellation_channels.py cc"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.set_order_cancellation_channels(var1,'Cancel')
print_results(p)
