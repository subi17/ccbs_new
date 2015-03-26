import re
import sys
from newton_init import *

#--------------------------------------------------
#     SEPARATE PRINT FOR THIS RPC RESULTS
#--------------------------------------------------
def print_results(pinput):
   for x in pinput:
      lod = len(x) 
      dpos = 1
      for k, v in x.items():
         print str(k) + ': ' + str(v)
         if (dpos == lod):
            print '-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.'
         dpos = dpos + 1

#--------------------------------------------------


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "No parameters!"
   print "Example: python newton_email_template_list.py"
   sys.exit()

if len(sys.argv) > 1: instruction()

p=s.newton.email_template.list()
print_results(p)
