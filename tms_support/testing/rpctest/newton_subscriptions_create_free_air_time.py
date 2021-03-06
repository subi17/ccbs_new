import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_subscriptions_create_free_air_time.py 10061512"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = date_time_builder('20131001')

p=s.newton.subscriptions.create_free_air_time(var1,
                                      'jannetou',
                                      {'fat_group':'IPL8CPACT',
                                      'period':"2017-01",
                                      'euro_amount':10.0,
                                      'user_euro_limit':20.0
                                      })
print_results(p)
