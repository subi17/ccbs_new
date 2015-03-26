import re
import sys
from ivr_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address,transport=p)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python ext_add_vip_order.py 633000431"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

q=s.ext.add_vip_order(
                              {'msisdn':var1,
                              'ICC':'8934040607000927455',
                              'subscription_type':'CONTRD',
                              'contractid':'contractid',
                              'offer_id':'TS000000CDPOS',
                              'subscription_bundle':'CONTD4',
                              'billing_data':'00491500012199999999'},
                              {'street':'CalleAgora',
                              'title':'Mr',
                              'fname':'Butch',
                              'lname':'Spencer',
                              'zip':'28003',
                              'region':'08',
                              'language':'es_ES',
                              'country':'ES',
                              'city':'MAD',
                              'id_type':'NIF',
                              'person_id':'74467657M',
                              'foundation_date':13/01/2005,
                              #'invoice_ref':'invoice_ref',
                              #'self_employed':True,
                              'birthday':06/03/1980,
                              'nationality':'Spanisih',
                              'sms_number':'622333333',
                              'street_code':'74',
                              'city_code':'74',
                              'email':'test.dude@qvantel.com'
                              #'order_inspection_rule':''
                              },
                              {'street':'CalleAgora',
                              'title':'Mr',
                              'fname':'Butch',
                              'lname':'Spencer',
                              'zip':'28003',
                              'region':'08',
                              'language':'es_ES',
                              'country':'ES',
                              'city':'MAD',
                              'id_type':'NIF',
                              'person_id':'74467657M',
                              'foundation_date':13/01/2005,
                              #'invoice_ref':'invoice_ref',
                              #'self_employed':True,
                              'birthday':12/03/1983,
                              'nationality':'Spanish',
                              'sms_number':'622333333',
                              'street_code':'74',
                              'city_code':'74',
                              'email':'test.dude@qvantel.com'
                              })
print_results(q)
