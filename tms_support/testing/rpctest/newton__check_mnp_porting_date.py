import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Porting date, Channel, Region"
   print "Example: python newton__check_mnp_porting_date 20140401 CC 99"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = date_time_builder(sys.argv[1])
   var2 = sys.argv[2]
   var3 = sys.argv[3]

p=s.newton.check_mnp_porting_date({"mnp_porting_date": var1,
                                   "order_channel": var2,
                                   "region": var3})
print_results(p)

