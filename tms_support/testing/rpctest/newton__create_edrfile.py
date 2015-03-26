import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
  print "Missing parameter: MSISDN, EDRFile, DateTime, "   
  print "Example: python newton__create_edrfile.py 600000174 edr_charge_nok.asc 20150202"
  sys.exit()

if len(sys.argv) < 4: instruction()
else:
	var1 = (sys.argv[1])
        var2 = (sys.argv[2])
	var3 = date_time_builder((sys.argv[3]))
	
	p=s.newton.create_edrfile(var1, var2, var3)
	print_results(p)
	
