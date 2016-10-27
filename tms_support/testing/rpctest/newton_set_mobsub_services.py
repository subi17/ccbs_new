import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_set_mobsub_services.py 10055111 on"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = sys.argv[2]

p=s.newton.set_mobsub_services(var1,'operator','ilkkasav',
                              [{'service_id':'C_BRAIC',
                                'value':var2,
                                'param':'FakeParam'
                               }],
                               {'title':'otsikko1',
                                 'content':'text under otsikko1'
                                 })



print_results(p)
