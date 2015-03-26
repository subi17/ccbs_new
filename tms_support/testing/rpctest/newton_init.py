import xmlrpclib
import datetime
import os
import re

# Check system
sysversion = re.sub('\n','',os.popen('hostname').read())
if sysversion == 'hebe':
   address = 'http://localhost:3001/fcgiproxy_xfera/'
elif sysversion == 'sadira' or sysversion == 'merga' or sysversion == 'botein' or sysversion == 'merak':
   address =  'http://localhost:3000/tmsrpc_xfera/'

def date_time_builder(pdate):
   # input 20130131
   resultdate = xmlrpclib.DateTime(datetime.datetime(int(pdate[:4]),int(pdate[4:6]),int(pdate[6:])).timetuple())
   return resultdate

