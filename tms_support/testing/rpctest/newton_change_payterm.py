import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_change_payterm.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.change_payterm(
                              {'msseq':var1,
                               'current_payterm':'PAYTERM18_10',
                               'new_payterm':'PAYTERM18_8',
                               'username':'vikasagr'},
                              {'title':'testing',
                               'content':'changingpayterm'}
                              )
print_results(p)
