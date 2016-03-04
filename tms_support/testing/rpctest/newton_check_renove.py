import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_check_renove.py 622689226"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.check_renove({'msisdn':var1,
                         'channel':'renewal'
#                         'person_id':'27388306K',
#                         'id_type':'NIF'
                        }
                       )
print_results(p)

