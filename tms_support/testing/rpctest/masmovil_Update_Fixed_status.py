import re
import sys
from masmovil_init import *
from show_results import *

# Definition
#s = xmlrpclib.ServerProxy(address)
s = xmlrpclib.ServerProxy(address, transport=p)
updtime =  datetime.datetime.now() + datetime.timedelta(minutes=5)
def instruction():
   print "Missing parameters"
   print "Example: python masmovil_Update_Fixed_status.py Y70037915 CERRADA"
   sys.exit()
if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = sys.argv[2]

if var2 == "CANCELADA":
   var3 = "cancelled with script"
else:
   var3 = ""
q = s.masmovil.Update_Fixed_status({
   "notificationID": "1",
   "notificationTime": updtime.isoformat(),
   "notificationType": "O",
   "orderID":var1,
   "Status": {
         "Status":var2,
         "StatusDescription": "hello",
         "lastDate": updtime.isoformat(),
         "additionalInfo": var3
   }

})

print_results(q)
