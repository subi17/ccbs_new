import re
import sys
from newton_init import *
from show_results import *


# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MsSeq"
   print "Example: python newton_acc_save.py 10055111"
   sys.exit()

if len(sys.argv) < 2: instruction()
else:
   var1 = int(sys.argv[1])
   var2 = date_time_builder('20130917')

p=s.newton.acc_save(var1,'vikasagr',var2,
                        {'lname':'Tourunen',
                         'fname':'Janne',
                         'street':'CALLE CALADERO 12',
                         'zip':'29006',
                         'city':'Malaga',
                         'region':'hereandthere',
                         'language':'1',
                         'nationality':'FI',
                         'person_id':'X1000200E',
                         'bankaccount':'0049999960999999',
                         'id_type':'NIE'},
                         3.0,5.0,
                         {'title':'Experiment',
                          'content':'Just an example'})
print_results(p)

