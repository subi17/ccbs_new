import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Person/Company ID"
   print "Example: python newton_acc_check_customer.py 31977628S"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
	var1 = sys.argv[1]

p=s.newton.acc_check_customer('NIF',var1)
print_results(p)
