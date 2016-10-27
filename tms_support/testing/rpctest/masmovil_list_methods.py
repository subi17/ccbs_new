import re
import sys
from masmovil_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
q = s.system.listMethods()

print_results(q)
