import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: orderid "
   print "Example: python newton_fixedline_installation_address_update.py 5032785"
   sys.exit()

print(sys.argv[0])
print(sys.argv[1])

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.fixedline_installation_address_update('rpc test',var1, 
						 {
                                       'city':'Finland',
                                       'coverage_token':'qwer',
                                       'gescal':'123',
                                       'region':'TENERIFE',
                                       'street_name':'WindyStreet 45',
                                       'street_number':'123',
                                       'territory_owner':'rpctest',
                                       'street_type':'WindyStreet 45',
                                       'zip':'123'},
				       'address change',
                                       'test contractid',
                                       'sample')


print_results(p)
