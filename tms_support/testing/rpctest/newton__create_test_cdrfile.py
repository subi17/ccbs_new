import re
import sys
from newton_init import *
from show_results import *

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "Missing parameter: MSISDN,[SecCLI], CDRFile, DateTime, Count, Meas, "
   print "Example: python newton__create_test_cdrfile.py 600000174 postpaid_nat_call.asc 20150202 10 300"
   sys.exit()

if len(sys.argv) < 6: instruction()
elif len(sys.argv) < 7:
	var1 = (sys.argv[1])
	var2 = (sys.argv[2])
	var3 = date_time_builder(sys.argv[3])
	var4 = int(sys.argv[4])
	var5 = int(sys.argv[5])	
	
	p=s.newton.create_test_cdrfile(var1,'',var2,var3,var4,var5)
	print_results(p)
else:
	var1 = (sys.argv[1])
	var2 = (sys.argv[2])
	var3 = (sys.argv[3])
	var4 = date_time_builder(sys.argv[4])
	var5 = int(sys.argv[5])
	var6 = int(sys.argv[6])
	
	p=s.newton.create_test_cdrfile(var1, var2, var3, var4, var5, var6);
	print_results(p)
	
