import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: ChargeEvent"
   print "Example: python newton_charge_events_delete.py ERKKI"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.charge_events.delete(var1,{'username':'vikasagr'})
print_results(p)
