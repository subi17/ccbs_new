/* ----------------------------------------------------------------------
  MODULE .......: dumpall.p
  TASK .........: dump all fields for tables
  APPLICATION ..: tms
  AUTHOR .......: tk
  CREATED ......: 07.12.06
  CHANGED ......: 07.12.06 jl/ Placed in production
                  22.03.07 kl invoice,orderpayment
                  28.03.07 kl dbtools.cls, WEEKDAY
                  22.05.07 jp added ordercustomer
                  
  Version ......: Yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcbrand = "1".
katun = "cron".
{Func/cparam2.i}

DEFINE VARIABLE lcFiles  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAreas  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcArea   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop   AS INTEGER   NO-UNDO.
DEFINE VARIABLE clsTools AS dbtools   NO-UNDO.

clsTools = NEW dbtools().

ASSIGN 
   SESSION:NUMERIC-FORMAT = "AMERICAN"
   lcFiles = "msowner".
  
DO liLoop = 1 to NUM-ENTRIES(lcFiles):

   ASSIGN
      lcFile = ENTRY(liLoop,lcFiles)
      lcArea = clsTools:FindDb(lcFile).
   
   RUN Mm/dumpall.i lcArea lcFile.
   
END.
