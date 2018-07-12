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
                                   'bis': '',
                                   'block': '2',
                                   'gescal': '00332432043241010         0085       ',
                                   'region': 'MADRID',
                                   'city': 'Galapagar',
                                   'street_name': 'Calle del Bosque',
                                   'street_number': '33',
                                   'territory_owner': 'MASMOVIL',
                                   'coverage_token': '19384392-0395-0982-9893-784379230910',
                                   'hand': 'E',
                                   'door': '',
                                   'letter': '',
                                   'km': '',
                                   'floor': '002',
                                   'stair': '',
  				   'zip': '28260',
  				   'street_type': 'Calle',
  			           'country': 'ES'
				  },
				  'FixedLineAddress',
				  'Customers request')

print_results(p)
