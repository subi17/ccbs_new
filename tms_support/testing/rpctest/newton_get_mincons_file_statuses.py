import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "No parameters allowed"
   print "Example: python newton_get_mincons_file_statuses"
   sys.exit()

if len(sys.argv) < 1: instruction()
else:
   p=s.newton.get_mincons_file_statuses()
   print_results(p)
