import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "No arameters"
   print "Example: newton_show_active_test_subscriptions__list.py"
   sys.exit()

if len(sys.argv) > 1: instruction()

p=s.newton.show_active_test_subscriptions.list(5,5)
print_results(p)
