import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: orderid "
   print "Example: python newton_set_installation_address.py 5032785"
   sys.exit()

print(sys.argv[0])
print(sys.argv[1])


if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.set_customer_address('rpc test',var1,{'street_name':'WindyStreet 45',
                                       'zip':'11500',
                                       'Region':'24',
                                       'city':'Madrid'},'rpc test')
print_results(p)
