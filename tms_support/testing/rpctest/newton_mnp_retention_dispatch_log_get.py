import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "No parameters!"
   print "Example: python newton_mnp_retention_dispatch_log_get.py"
   sys.exit()

if len(sys.argv) > 1: instruction()

p=s.newton.mnp_retention_dispatch_log.get("yoigo")
print_results(p)
