import re
import sys
from newton_init import *
from show_results import *

var1 = date_time_builder('20140421')
# Definition
s = xmlrpclib.ServerProxy(address)
p=s.newton.resellers.set({
   "id":"AL",
   "username":"anttis",
#   "name":"ALCAMPO",
   "address":"Street 2",
#   "name":"Antti S",
#   "bank_code_new":"0081",
#   "username":"anttis"
    "email":"test@test.com",
    "bank_code_new":"0049",
    "bank_code_new_date":var1
   })
print_results(p)
