import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_add_reactivation_request.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = date_time_builder('20130515')

p=s.newton.add_reactivation_request({'msseq':var1,
                        'salesman':'vikasagr',
                        'ActStamp':var2},
                        {'title':'MobSubReactivation',
                        'content':'MobSubReactivation'}
                        )
print_results(p)
