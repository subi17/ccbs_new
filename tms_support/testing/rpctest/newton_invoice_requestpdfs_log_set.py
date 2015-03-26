import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN"
   print "Example: python newton_invoice_requestpdfs_log_set.py 622689226"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = sys.argv[1]

p=s.newton.invoice_requestpdfs_log_set({
                             'username':'vikasagr',
                             'systemid':'Ext.SelfService_API',
                             'eventtype':'MiYoigo_password_change',
                             'ReasonCode':5,
                             'cli':var1,
                             'invnum':'TIVA01093978',
                             'accesstype':'r'})
print_results(p)
