import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_mnp_processes__list.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.mnp_processes.list({'mnp_type':1,
                                'salesman_id':'PH005001',
                                'msseq':var1})
                              
print_results(p)
