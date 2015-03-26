import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Name"
   print "Example: python newton_configurations_set.py provisioning"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.configurations.set(var1,
                {'value':1,
                 'user':
                 {'username':'vikasagr',
                 'name':'Janne',
                 'email':'user@host.com',
                 'phone_number':'0123456789'}
                })
print_results(p)
