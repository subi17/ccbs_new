import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSIDSN"
   print "Example: python newton_set_icc.py 722799945"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.set_icc(var1,'Paris','8934070000000054186',0.0,0.0,'','','BEEB')
print_results(p)
