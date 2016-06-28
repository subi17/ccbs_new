import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: salesmanid"
   print "Example: python newton_get_contract_details.py 722806158 '' 5514878 WEB 20160601"
   sys.exit()

if len(sys.argv) < 6: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]
   var3 = sys.argv[3]
   var4 = sys.argv[4]
   var5 = date_time_builder(sys.argv[5])

p=s.newton.get_contract_details(var1,var2,var3,var4,var5).
print_results(p)
