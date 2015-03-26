import xmlrpclib, httplib
import base64
import datetime
import time
bdate = xmlrpclib.DateTime(datetime.datetime(2002,6,3).timetuple())

username = "viptool"
#username = "external"
password = "huhhahhei"

class CustomTransport(xmlrpclib.Transport):
   def send_host(self, connection, host):
       connection.putheader('X-EndUserIdentification', 'Rafael')
       base64string = base64.encodestring('%s:%s' % (username, password))[:-1]
       authheader =  "Basic %s" % base64string
       connection.putheader("Authorization", authheader)

p = CustomTransport()
s = xmlrpclib.Server('http://localhost:3040/viptool', transport=p)
#q = s.ext.subscription.get_billing_permission(721691)
#q = s.ext.subscription.set_billing_permission(570288,2)
#q = s.ext.subscription.set_billing_permission(321170,2)
#q = s.ext.subscription.get_id("608086185")

#q = s.ext.subscription.topup_query("600003926")

#q = s.ext.bundles_set(10045858,"MDUB","on")
q = s.ext.bundles_get(10045458,"MDUB")

#q = s.ext.add_vip_order(
#                         {'msisdn':'622003622',
#                         'icc': '8934040107000250156',
#                         'offer_id':'G0000001CONT',
#                         'subscription_type':'CONT',
#                         'contractid':'TestVip',
#                         'billing_data':'55555'},

#                        {'fname':'lolo',
#                         'lname':'vivi',
#                         'lname2':'soso',
#                         'title':'Sr Lolo',
#                         'region':'09',
#                         'street':'calle 56',
#                         'zip':'2020',
#                         'city':'Madrid',
#                         'country':'ES',
#                         'street_code':'3245',
#                         'city_code':'2222',
#                         'nationality':'ES',
#                         'birthday':bdate,
#                         'email':'bei@cc.com',
#                         'sms_number':'123456',
#                         'phone_number':'444444',
#                         'language':'en',
#                         'person_id':'C342E',
#                         'id_type':'CIE',
#                         'mark_sms':True,
#                         'mark_sms_3rd':False,
#                         'mark_post':False,
#                         'mark_post_3rd':True,
#                         'mark_email':False,
#                         'mark_email_3rd':True}
#                        )

#q = s.ext.get_free_numbers("vip")

#q = s.ext.subscriptions.charge({'msseq':771229,
#                                  'charge_event_id':'STC_POSTPAID',
#                                  'amount':12.11,
#                                  'charge_limit':20.0})
print q


