{Syst/tmsconst.i}
DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,{&HOSTNAME_STAGING}) EQ 0 AND
   LOOKUP(lcHostName,{&HOSTNAME_DEVEL}) EQ 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
      lcHostName VIEW-AS ALERT-BOX.
END.
ELSE DO:
   FOR EACH CallAlarm WHERE
            CallAlarm.brand EQ Syst.Var:gcBrand AND
            CallAlarm.delistat EQ {&CA_DELISTAT_NEW} AND
            CallAlarm.delitype EQ 1 /* SMS */:
      ASSIGN
         CallAlarm.delistamp = Func.Common:mMakeTS()
         CallAlarm.delistat = {&CA_DELISTAT_SENT}.
   END.         
            
   
END.
