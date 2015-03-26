# -*- coding: utf-8 -*- 
import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_sms_template__test.py 622689226"
   sys.exit()

# To change sms content, change text below.
sms_content = "Hola.ElsaldodecompensacionquenossolicitasteyaestáentuTarjeta.AestasalturasyasabrásquehacertedeYoigofueunabuenaidea.Verdad?62259055"
if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.sms_template.test(var1,sms_content)
print_results(p)
