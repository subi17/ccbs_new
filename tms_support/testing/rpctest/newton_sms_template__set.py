# -*- coding: utf-8 -*- 
import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: SMS Keyvalue"
   print "Example: python newton_sms_template__set.py DATA5DeActMan"
   sys.exit()

# To change sms content, change text below.
if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.sms_template.set('jannetou',var1,
                           [
                           {'language':5,
                            'smstext':'Experimentat to be removed'
                           }])
                           
print_results(p)
