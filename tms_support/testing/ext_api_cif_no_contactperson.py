import xmlrpclib, httplib
import base64
import datetime
import time
bdate = xmlrpclib.DateTime(datetime.datetime(2002,6,3).timetuple())

username = "external"
password = "huhhahhei"


class CustomTransport(xmlrpclib.Transport):
   def send_host(self, connection, host):
       connection.putheader('X-EndUserIdentification', 'Rafael')
       base64string = base64.encodestring('%s:%s' % (username, password))[:-1]
       authheader =  "Basic %s" % base64string
       connection.putheader("Authorization", authheader)

p = CustomTransport()
s = xmlrpclib.Server('http://external:external@localhost:3040/external/', transport=p)


q = s.ext.add_vip_order(
                         {'msisdn':'622000181',
                         'icc': '8934040307000766728',
                         'offer_id':'G0000001CONT',
                         'subscription_type':'CONT',
                         'contractid':'TestVip',
                         'billing_data':'55555'},

                        {'fname':'Lord',
                         'lname':'Orpsis',
                         'lname2':'',
                         'title':'Sr',
                         'site_name':'Pro-tan',
                         'foundation_date':bdate,
                         'region':'05',
                         'street':'calle 556',
                         'zip':'52520',
                         'city':'Madrid',
                         'country':'ES',
                         'street_code':'32',
                         'city_code':'2222',
                         'nationality':'ES',
                         'birthday':bdate,
                         'email':'bei@cc.com',
                         'sms_number':'123456',
                         'phone_number':'444444',
                         'language':'en',
                         'person_id':'10000000G',
                         'company_id': 'X8888888X',
                         'id_type':'NIF',
                         'mark_sms':True,
                         'mark_sms_3rd':False,
                         'mark_post':False,
                         'mark_post_3rd':True,
                         'mark_email':False,
                         'mark_email_3rd':True},

                        {}
                           
                        ) 


print
