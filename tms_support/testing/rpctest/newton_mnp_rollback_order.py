import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_mnp_rollback_order.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.mnp_rollback_order(
                              {'id_type':'NIF',
                              'person_id':'74467657M',
                              'old_operator':'vodafone'
                              },
                              [{'msseq':var1,
                              'contract_id':'CONT1245',
                              'old_operator_paytype':'prepaid'
                              }]
                              )
                              
print_results(p)
