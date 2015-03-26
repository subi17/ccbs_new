import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: ChargeEvent"
   print "Example: python newton_charge_events__get.py FUTURE-1"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

#p=s.newton.charge_events.get([var1,var2,var3,...])
p=s.newton.charge_events.get([var1])
print_results(p)
