import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_set_mobsub_services.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = date_time_builder('20130801')

p=s.newton.set_mobsub_services(var1,'operator','jannetou',
                              [{'service_id':'VMS',
                                'value':'on',
                                'param':'FakeParam'
                               }],
                               {'title':'otsikko1',
                                 'content':'text under otsikko1'
                                 })
print_results(p)
