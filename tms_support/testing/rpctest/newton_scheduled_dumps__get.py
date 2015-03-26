import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: DumpId"
   print "Example: python newton_scheduled_dumps__get.py 29"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.scheduled_dumps.get({'dumpid':var1})
print_results(p)
