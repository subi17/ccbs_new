import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Request Id"
   print "Example: python newton_invoices__activate_email.py 50248349"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.invoices__activate_email(var1,'43eec3e014a82966b6f5609113ac1091d516a0d2')
print_results(p)
