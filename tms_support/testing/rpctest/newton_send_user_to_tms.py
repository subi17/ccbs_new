import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Username"
   print "Example: python newton_send_user_to_tms.py Paris"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.send_user_to_tms(var1,'Paris Hilton','testing',True,'12')
print_results(p)
