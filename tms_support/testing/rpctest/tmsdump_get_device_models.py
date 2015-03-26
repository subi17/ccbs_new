import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "No parameters!"
   print "Example: python tmsdump_get_device_models.py"
   sys.exit()

if len(sys.argv) > 1: instruction()

p=s.tmsdump.get_device_models()
print_results(p)
