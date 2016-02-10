/* ----------------------------------------------------------------------
  MODULE .......: giftorderrepb.p
  TASK .........: Print a report from gift orders, batch-version
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 08.11.07
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "Cron".
   
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/ftransdir.i}

DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR lcSpoolDir    AS CHAR NO-UNDO.
DEF VAR lcTransDir    AS CHAR NO-UNDO.

ASSIGN 
   lcFile     = fCParamC("GiftOrderRepFile")
   lcSpoolDir = fCParamC("GiftOrderRepSpool")
   lcTransDir = fCParamC("GiftOrderRepTrans").
       
IF lcFile = "" OR lcFile = ? THEN DO:
   fELog("GIFTORDERREPORT","ERROR:File_not_defined").
   RETURN.
END.

fELog("GIFTORDERREPORT","Started").

ASSIGN 
   lcFile = REPLACE(lcFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                   STRING(MONTH(TODAY),"99") + 
                                   STRING(DAY(TODAY),"99"))
   lcFile = REPLACE(lcFile,"#TIME",REPLACE(STRING(TIME,"hh:mm:ss"),":",""))
   lcFile = lcSpoolDir + "/" + lcFile.

RUN Mm/readorderrep (TODAY,
                  "yoigo",  
                  "gift",
                  lcFile,
                  OUTPUT liCount).

IF lcTransDir > "" THEN DO:
   fTransDir(lcFile,
             ".dump",
             lcTransDir).
END.

fELog("GIFTORDERREPORT","Ended:" + STRING(liCount)).


