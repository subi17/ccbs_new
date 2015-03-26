import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: CriteriaType"
   print "Example: python newton_offer_criteria_add.py CLIType"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = date_time_builder('20121210')

p=s.newton.offer_criteria.add({'username':'vikasagr',
                             'criteria_type':var1,
                             'offer_id':'TS0000POSTVP',
                             'valid_from':var2
                            })
print_results(p)
