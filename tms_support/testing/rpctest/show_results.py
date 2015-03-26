# This module adjust printing
# This works now pretty well, if need modification, take a copy before changes.
import re

def print_results(pinput):
   if type(pinput) == dict:
      lod = len(pinput) 
      dpos = 1
      for k, v in pinput.items():
         if type(v) == list:
            print str(k)
            listanalyse(v)
         #elif re.search('{',v):
         #   print str(k)
         #   print re.sub('{','\n{',v)
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
