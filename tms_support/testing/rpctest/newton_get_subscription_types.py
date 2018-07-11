import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Example: python newton_get_subscription_types.py CONTDSL40 DATA6"
   sys.exit()

if len(sys.argv) == 1: 
   var1 = ""
   var2 = ""
elif len(sys.argv) == 2:
   var1 = sys.argv[1]
   var2 = ""
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]

p=s.newton.get_subscription_types('yoigo',{'cli_type':var1,
                                   'bundle_id':var2})

print_results(p)
