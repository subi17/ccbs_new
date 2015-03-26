import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CustNum"
   print "Example: python newton_subscriptions_search_terminated.py 1959964"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.subscriptions.search_terminated({'search_type':'custnum',
                                            'search_key':var1,
                                            'limit':4,
                                            'offset':1,
                                            'admin':True})
print_results(p)
