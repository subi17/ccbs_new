import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Person/Company ID"
   print "Example: python newton_check_customer.py <yoigo/masmovil> 83658423V NIF CONT10"
   sys.exit()

if len(sys.argv) < 5: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]
   var3 = sys.argv[3]
   var4 = sys.argv[4]

p=s.newton.check_customer(var1,var2,var3,True,1,var4,"VFR",True)
print_results(p)
