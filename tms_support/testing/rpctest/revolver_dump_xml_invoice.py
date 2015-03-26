import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: InvNum"
   print "Example: python revolver_dump_xml_invoice.py 10848062"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.revolver.dump_xml_invoice(var1,'FOLDER')
print_results(p)
