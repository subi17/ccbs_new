import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_customers__set_deliverables.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])


p=s.newton.customers__set_deliverables({'customer_id':3925729,
                                        'username':'vikasagr',
                                        'itemizations':
                                        [{'msseq':var1,
                                          'callspec':False
                                         }]})
print_results(p)


