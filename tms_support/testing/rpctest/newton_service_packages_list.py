import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: start of ServPac name"
   print "Example: python newton_service_packages_list.py UN"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.service_packages.list({'id_begins':var1})
print_results(p)
