import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: EventType"
   print "Example: python newton_events_list.py stc"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = date_time_builder('20121001')
   var3 = date_time_builder('20121101')

p=s.newton.events.list({'username':'vikasagr',
                        'date_start':var2,
                        'date_end':var3,
                        'event_type':var1})
                              
print_results(p)
