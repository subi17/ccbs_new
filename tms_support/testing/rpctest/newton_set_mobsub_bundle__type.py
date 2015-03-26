import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_set_mobsub_bundle__type.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = date_time_builder('20130801')

p=s.newton.set_mobsub_bundle_type(
                              {'msseq':var1,
                              'old_bundle':'CONTF55',
                              'new_bundle':'CONTF20',
                              'date':var2,
                              'username':'vikasagr'
                              })
print_results(p)
