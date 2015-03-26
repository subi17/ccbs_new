import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "No parameters!"
   print "Example: python newton_imeis_to_be_released.py"
   sys.exit()

if len(sys.argv) > 1: instruction()

p=s.newton.imeis.to_be_released()
print_results(p)
