import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: Person/Customer ID"
   print "Example: python newton_set_customer_details.py 3717582"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.set_customer_details(var1,'selfcare',True,{'title':'Mr',
	"street":"street_1",
	"city":"city_2",
 	"country":"country_2",
	"zip":"34013",
	"region":"04",
	"coname":"coname_1",
	"city_code":"125"
#	"street_code":"456",
#	"municipality_code":"789"}
},{})

print_results(p)
