import re
import sys
from masmovil_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
def instruction():
   print "Missing parameters"
   print "Example: python masmovil_Update_Fixed_status.py Y70037915 CERRADA"
   sys.exit()
if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]

q = s.masmovil.Update_Fixed_status({
   "notificationID": "1",
   "notificationTime": date_time_builder('20160922'),
   "notificationType": "1",
   "orderID":var1,
   "Status": {
         "Status":var2,
         "StatusDescription": "hello",
         "lastDate": date_time_builder('20160922')
   }

})

print_results(q)
