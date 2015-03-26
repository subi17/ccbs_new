import xmlrpclib
import datetime

# Merak - Staging address dextra:
address = 'http://localhost:3010/tmsrpc_xfera"/'

def date_time_builder(pdate):
   # input 20130131
   resultdate = xmlrpclib.DateTime(datetime.datetime(int(pdate[:4]),int(pdate[4:6]),int(pdate[6:])).timetuple())
   return resultdate
