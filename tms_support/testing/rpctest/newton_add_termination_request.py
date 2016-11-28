import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq timeST TerminationType(Full/Partial)"
   print "Example: python newton_add_termination_request.py 10055111 20161130 Full"
   sys.exit()

if len(sys.argv) < 4: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = date_time_builder(sys.argv[2])
   var3 = sys.argv[3]

p=s.newton.add_termination_request({'msseq':var1,
                        'salesman':'rpctest',
                        'killts':var2,
                        'orderer':1,
                        'opcode':745123,
                        'termination_type':var3}
                        )
print_results(p)
