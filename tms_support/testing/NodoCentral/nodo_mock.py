#!/usr/bin/python
# -*- coding: latin-1 -*-
import sys
def instruction():
   print "Usage:"
   print 'python nodo_mock.py "testinput.txt" 0 > testoutput.txt"'

if len(sys.argv) < 3: instruction()
else:
   inputfile = sys.argv[1]
   option = sys.argv[2]
   f = open(inputfile, 'r')

   for line in f:
       txt = line.strip()
       if txt == '':
           sys.stdout.write('\n')
           sys.stdout.flush()
       elif option == '1':
          sys.stdout.write( txt + ';MNMO NPROP;a\n')
       elif option == '2':
          sys.stdout.write( txt + ';MNMO NUMPO;b\n')
       else:
          sys.stdout.write( txt + ';0000 00000;La operación se ha realizado con éxito. \n')

    
       sys.stdout.flush()

#   f.close()
