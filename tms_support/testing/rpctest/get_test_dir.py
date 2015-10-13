import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
p=s.newton.get_test_dir()
print_results(p)

