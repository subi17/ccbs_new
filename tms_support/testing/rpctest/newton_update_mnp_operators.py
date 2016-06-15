import re
import sys
from newton_init import *
from show_results import *

# RUN THIS by these commands !!!!
# python newton_update_mnp_operators.py > resultfile.txt
# vim resultfile.txt  and type :s/, /\r/g too see report properly

# Definition
s = xmlrpclib.ServerProxy(address)
def instruction():
   print "No parameters!"
   print "Example python newton_update_mnp_operators.py > resultfile.txt" 
   sys.exit()

if len(sys.argv) > 1: instruction()

p=s.newton.update_mnp_operators()
# Instead of normal printing
# print_results(p)
# print all to file and check the file, like explained above.
print p
