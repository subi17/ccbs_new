import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: DumpID"
   print "Example: python newton_dump_status__get.py 66"
   sys.exit()

var2 = date_time_builder('20140901')
var3 = date_time_builder('20140915')

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.dump_status.get({'starttime':var2,'endtime':var3,'dumpid':var1})
                              
print_results(p)
