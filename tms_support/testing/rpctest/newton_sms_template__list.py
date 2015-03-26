import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Invalid parameters"
   print "No parameter values"
   sys.exit()

if len(sys.argv) > 1: instruction()

p=s.newton.sms_template.list()
print_results(p)
