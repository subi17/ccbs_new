import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_get_mobsub_balance.py 10055111"
   sys.exit()

if len(sys.argv) < 1: instruction()
#else:
#   var1 = sys.argv[1]

p=s.newton.bundles.get(["OFFICE365|yoigo"])
print_results(p)
