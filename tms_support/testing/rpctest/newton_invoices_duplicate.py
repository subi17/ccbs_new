import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: InvNum"
   print "Example: python newton_invoices__duplicate.py 10847956"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.invoices__duplicate(var1,
                {'username':'vikasagr',
                 'memo':
                {'title':'This is title',
                 'content':'This is content for memo'}
                })
print_results(p)
