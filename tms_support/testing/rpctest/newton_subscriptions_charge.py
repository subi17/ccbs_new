import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_subscriptions_charge.py 10061512"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])

p=s.newton.subscriptions.charge({'msseq':var1,
                                      'charge_event_id':'FeeModel',
                                      'amount':4.00,
                                      'username':'jannetou',
                                      'charge_limit':5.00
                                      })
print_results(p)
