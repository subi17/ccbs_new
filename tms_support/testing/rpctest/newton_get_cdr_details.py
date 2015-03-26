import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_get_cdr_details.py 10055111 20150101 20150131"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = date_time_builder(sys.argv[2])
   var3 = date_time_builder(sys.argv[3])

p=s.newton.get_cdr_details(var1,var2,var3,'normal','miyoigo')
print_results(p)
