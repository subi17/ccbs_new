import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_add_memo.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.add_memo('23532','CustNum',23532,
                    'TermSub','Periodical Contract',
                    'YOIGOYOIGO: Terminated along with the subscription',
                    0)
print_results(p)
