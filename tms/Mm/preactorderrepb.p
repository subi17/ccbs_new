/* ----------------------------------------------------------------------
  MODULE .......: preactorderrepb.p
  TASK .........: Print a report from preactivated orders, batch-version
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 13.11.07
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "Cron".
   
{cparam2.i}
{eventlog.i}
{ftransdir.i}

DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR lcSpoolDir    AS CHAR NO-UNDO.
DEF VAR lcTransDir    AS CHAR NO-UNDO.

ASSIGN 
   lcFile     = fCParamC("PreActOrderRepFile")
   lcSpoolDir = fCParamC("PreActOrderRepSpool")
   lcTransDir = fCParamC("PreActOrderRepTrans").
       
IF lcFile = "" OR lcFile = ? THEN DO:
   fELog("PREACTORDERREPORT","ERROR:File_not_defined").
   RETURN.
END.

fELog("PREACTORDERREPORT","Started").

ASSIGN 
   lcFile = REPLACE(lcFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                   STRING(MONTH(TODAY),"99") + 
                                   STRING(DAY(TODAY),"99"))
   lcFile = REPLACE(lcFile,"#TIME",REPLACE(STRING(TIME,"hh:mm:ss"),":",""))
   lcFile = lcSpoolDir + "/" + lcFile.

RUN readorderrep (TODAY,
                  "pre-act",
                  "yoigo",
                  lcFile,
                  OUTPUT liCount).

IF lcTransDir > "" THEN DO:
   fTransDir(lcFile,
             ".dump",
             lcTransDir).
END.

fELog("PREACTORDERREPORT","Ended:" + STRING(liCount)).


