import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: FatGroup"
   print "Example: python newton_fat_schemes_get.p BONO12CPACT"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.fat_schemes.get([var1])
                              
print_results(p)
