import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "No parameters!"
   print "Example: python newton_topup_schemes_list.py"
   sys.exit()

if len(sys.argv) > 1: instruction()

p=s.newton.topup_schemes.list({})
print_results(p)
