import re
import sys
from dextra_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)

fdate = xmlrpclib.DateTime(datetime.datetime.now().timetuple())

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: OrderId"
   print "Example: python dextra_update_order_status.py 10088467"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p = s.dextra.update_order_status(
   {
      'Order_Id':var1,
      'timestamp':fdate,
      'LO_Id':1,
      'LO_Description':"Dextra",
      'LO_Status_ID':1,
      'LO_Status_Description':"Order Received by LO",
      'Courier_ID':0,
      'Courier_Description': "Dextra",
      'Courier_Shipping_ID':"ABC",
#      'IMEI_COL':[{'MSDN':'342343','IMEI':'002344000000018'}],
      'delivery_address':{
         'region':'29',
         'street':'Street',
         'zip':'29590',
         'city':'Barcelona',
         'country':'ES',
         'street_code':'1234',
         'city_code':'5678'
   }
   }
)

print_results(p)
