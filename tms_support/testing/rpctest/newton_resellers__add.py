import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
p=s.newton.resellers.add({
   "id":"AS",
   "name":"Antti S",
   "bank_code_new":"0081",
   "username":"anttis"
   })
print_results(p)
