import re
import sys
from selfservice_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python ext_selfservice__add_memo.py 622689226"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

q=s.ext.selfservice.add_memo('509','', var1, "Test Title", "Test Content", "MobSub")
print_results(q)

