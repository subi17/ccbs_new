import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_subscriptions_create_discount_member.py 10061512"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = date_time_builder('20131001')

p=s.newton.subscriptions.create_discount_member(var1,
                                      'jannetou',
                                      {'id':'DEMODISC30',
                                       'disc_value':5.0,
                                      'valid_from':var2,
                                      'valid_periods':4,
                                      'discount_monthly_limit':25.0
                                      },
                                      True)
print_results(p)
