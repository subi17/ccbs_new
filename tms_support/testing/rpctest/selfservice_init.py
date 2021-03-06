import xmlrpclib
import datetime
import httplib
import base64
import sys,math

# Merak - Staging address selfservice:
address = 'http://localhost:3060/selfservice/'

username = "selfservice"
password = "selfservice"

class CustomTransport(xmlrpclib.Transport):
   def send_host(self, connection, host):
       connection.putheader('X-EndUserIdentification', 'QBS Tester')
       base64string = base64.encodestring('%s:%s' % (username, password))[:-1]
       authheader =  "Basic %s" % base64string
       connection.putheader("Authorization", authheader)

p = CustomTransport()


def date_time_builder(pdate):
   # input 20130131
   resultdate = xmlrpclib.DateTime(datetime.datetime(int(pdate[:4]),int(pdate[4:6]),int(pdate[6:])).timetuple())
   return resultdate

