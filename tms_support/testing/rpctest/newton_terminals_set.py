import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Terminal id"
   print "Example: python newton_terminals_set.py 39798"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.terminals.set(var1,{'username':'jannetou'})
print_results(p)
