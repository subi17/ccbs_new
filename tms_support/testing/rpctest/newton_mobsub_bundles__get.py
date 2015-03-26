import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_mobsub_bundles__get.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

param1 = 'TARJ_UPSELL' + '|' + var1
p=s.newton.mobsub_bundles__get([param1])
                              
print_results(p)
