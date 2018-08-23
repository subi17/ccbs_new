import re
import sys
from newton_init import *
from show_results import *
import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: orderid "
   print "Example: python newton_fixedline_order_update.py 5032785"
   sys.exit()

print(sys.argv[0])
print(sys.argv[1])

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.fixedline_order_update('srvuddan',var1,
                                  {
                                   'bis': '124',
                                   'block': '2',
                                   'gescal': '28000420766400002         002E',
                                   'region': 'MADRID',
                                   'city': 'Galapagar',
                                   'street_name': 'Calle del Bosque',
                                   'street_number': '33',
                                   'territory_owner': 'MDMM01',
                                   'coverage_token': '19384392-0395-0982-9893-784379230910',
                                   'hand': 'E',
                                   'address_id':'28000420766400002         002E',
                                   'door': '45',
                                   'letter': '28',
                                   'km': '6',
                                   'floor': '002',
                                   'stair': '12',
                                   'zip': '28260',
                                   'street_type': 'Calle',
                                   'country': 'ES'
                                  },
                                  'ChangeInstallationAddress',
                                  'Customers request')

print_results(p)

