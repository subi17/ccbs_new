
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 13.08.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 

DEFINE VARIABLE lcFiles AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO.
lcFiles = "XFE00330.OUT XFE00331.OUT XFE00332.OUT XFE00333.OUT XFE00334.OUT XFE00335.OUT XFE00336.OUT".
DEFINE VARIABLE lcNewStock AS CHARACTER NO-UNDO.
def stream sout.
output stream sout to /apps/snet/200809/as_ycm900.log.

lcNewStock = "RETAILER".

DO i = 1 TO NUM-ENTRIES(lcFiles, " "):
   lcFile = ENTRY(i,lcFiles," ").
   FOR EACH simbatch where 
      simbatch.brand = "1" AND
      simbatch.filename = TRIM(lcFile) NO-LOCK:
      FOR EACH sim where
         sim.brand = "1" and
         sim.simbatch = simbatch.simbatch EXCLUSIVE-LOCK:
         
         
         put stream sout unformatted sim.icc " " sim.stock "->" lcNewStock " " lcFile skip.
         assign sim.stock = lcNewStock. 
      END.
   END.

END.

lcNewStock = "RETAILER".

lcFiles = "XFE00337.OUT XFE00338.OUT XFE00339.OUT".

DO i = 1 TO NUM-ENTRIES(lcFiles, " "):
   lcFile = ENTRY(i,lcFiles," ").
   FOR EACH simbatch where 
      simbatch.brand = "1" AND
      simbatch.filename = TRIM(lcFile) NO-LOCK:
      FOR EACH sim where
         sim.brand = "1" and
         sim.simbatch = simbatch.simbatch EXCLUSIVE-LOCK:
         
         put stream sout unformatted sim.icc " " sim.stock "->" lcNewStock " " lcFile skip.
          assign sim.stock = lcNewStock. 
      END.
   END.

END.


output close.
