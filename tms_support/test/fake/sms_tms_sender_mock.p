{Syst/commpaa.i}
{Syst/tmsconst.i}
{Func/timestamp.i}
DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,'angetenar,alpheratz,sadachbia,yanai') = 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
      lcHostName VIEW-AS ALERT-BOX.
END.
ELSE DO:
   FOR EACH CallAlarm WHERE
            CallAlarm.brand EQ Syst.Parameters:gcBrand AND
            CallAlarm.delistat EQ {&CA_DELISTAT_NEW} AND
            CallAlarm.delitype EQ 1 /* SMS */:
      ASSIGN
         CallAlarm.delistamp = fmakets()
         CallAlarm.delistat = {&CA_DELISTAT_SENT}.
   END.         
            
   
END.
