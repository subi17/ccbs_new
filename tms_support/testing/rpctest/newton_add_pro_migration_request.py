import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq "
   print "Example: python newton_add_migration_request.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.add_pro_migration_request({'msseq':var1,
                        'salesman':'rpctest'}
                        )
print_results(p)
