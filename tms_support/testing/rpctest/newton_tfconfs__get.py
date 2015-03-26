import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
p=s.newton.tfconfs.get(["1","2","3","4","5"])
print_results(p)
