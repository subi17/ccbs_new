import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MNPOperationID"
   print "Example: python newton_mnp_messages_set.py 220885"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.mnp_messages.set(var1,
                           {'operation_id':'resend',
                            'username':'vikasagr',
                            'memo':{
                             'title':'Otsikko',
                             'content':'some content'}
                           })
print_results(p)
