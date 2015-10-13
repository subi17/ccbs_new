import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
var1 = int(sys.argv[1])
p=s.newton.test_email(var1)
print_results(p)
