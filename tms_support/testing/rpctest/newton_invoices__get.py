import re
import sys
from newton_init import *


# ------------------------------------------------------
# --   PRINT MODULE MODIFIED FOR THIS PARTICULAR TEST

def print_results(pinput):
   if type(pinput) == dict:
      lod = len(pinput) 
      dpos = 1
      for k, v in pinput.items():
         if type(v) == list:
            print str(k)
            listanalyse(v)
         elif re.search('{',v):
            print str(k)
            print re.sub('{','\n{',v)
         else: print str(k) + ': ' + str(v)
         if (dpos == lod):
            print '-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.'
         dpos = dpos + 1

   elif type(pinput) == list:
      listanalyse(pinput)
   else:
      print 'Result: ' + str(pinput)

def listanalyse(plist):
   printlist = []
   for xitem in plist:
      if type(xitem) == list:
         listanalyse(xitem)
      elif type(xitem) == dict:
          print_results(xitem)
      else:
         if xitem == '':
            printlist.append('')
         else:
            printlist.append(xitem)
   if len(printlist) > 0: print printlist
# ------------------------------------------------------


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Custnum"
   print "Example: python newton_invoices__get.py 700019751"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

#p=s.newton.invoices.get([var1,var2,var3,...])
p=s.newton.invoices.get([var1])
print_results(p)
