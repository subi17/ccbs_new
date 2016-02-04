import re
import sys
from dextra_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)

fdate = xmlrpclib.DateTime(datetime.datetime.now().timetuple())

# Definition
s = xmlrpclib.ServerProxy(address, transport=p)
def instruction():
   print "Missing parameter: OrderId LogisticStatus"
   print "Example: python dextra_update_order_status.py 70003913 1"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]
   var2 = int(sys.argv[2])

q = s.dextra.update_order_status(
   {
      'Order_Id':var1,
      'timestamp':fdate,
      'LO_Id':1,
      'LO_Description':"Netkia",
      'LO_Status_ID':var2,
      'LO_Status_Description':"",
      'Courier_ID':0,
      'Courier_Description': "ASM",
      'Courier_Shipping_ID':"ABC"
#      'IMEI_COL':[{'MSDN':'342343','IMEI':'002344000000018'}],
#      'delivery_address':{
#         'region':'29',
#         'street':'Street',
#         'zip':'29590',
#         'city':'Barcelona',
#         'country':'ES',
#         'street_code':'1234',
#         'city_code':'5678'
#   }
   }
)

print_results(q)
