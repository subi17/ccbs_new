import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CLIType"
   print "Example: python newton_subscription_creation_tool.py CONTF30"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.subscription_creation_tool('jannetou',
                                      'firstname.lastname@host.postfix',
                                     {'custid_type':'NIF',
                                      'subs_types':var1})
print_results(p)
