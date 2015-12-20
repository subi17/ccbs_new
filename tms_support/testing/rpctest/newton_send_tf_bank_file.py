import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Username bankcode"
   print "Example: python newton_send_bank_file.py jannetou 0049"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]

p=s.newton.send_tf_cancel_bank_file(var1,var2)
print_results(p)
