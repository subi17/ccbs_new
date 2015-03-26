import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "No parameters!"
   print "Example python newton_sms_template__logs.py"
   sys.exit()

if len(sys.argv) > 1: instruction()

p=s.newton.sms_template.logs()
print_results(p)
