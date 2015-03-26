import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: DumpID DumpLog_Filename"
   print "Example: python newton_dump_status__get.py 66 'incorrect_cust_data_20150112.dump'"
   sys.exit()


if len(sys.argv) < 3: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = sys.argv[2]

p=s.newton.dump_resend(var1,var2)
                              
print_results(p)
