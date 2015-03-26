import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OrderId"
   print "Example: python newton_mnp_relaunch.py 10083981"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.mnp_relaunch({'order_id':var1,
                              'old_operator':'Orbitel',
                              'old_icc':'8934040108008033875',
                              'id_type':'NIF',
                              'customer_id':'76258464D',
                              'username':'vikasagr',
                              'surname1':'Murizo',
                              'first_name':'Stefan',
                              'memo':
                              {'title':'otsikko',
                               'content':'words come not easy'
                              }
                             })
print_results(p)
