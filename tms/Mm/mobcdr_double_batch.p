/* -----------------------------------------------
  MODULE .......: mobcdr_double_batch.p
  FUNCTION .....: find double cdrs (batch)
  AUTHOR .......: aam
  CREATED ......: 28.10.11
------------------------------------------------------ */

{Syst/commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun = "cron".
{Func/lib/eventlog.i}
{Func/timestamp.i}
{Func/cparam2.i}

DEF VAR liMarked    AS INT  NO-UNDO.
DEF VAR ldaReadDate AS DATE NO-UNDO.
DEF VAR lcLogFile   AS CHAR NO-UNDO. 

IF SESSION:PARAMETER > "" THEN 
   ldaReadDate = DATE(SESSION:PARAMETER) NO-ERROR.
IF ldaReadDate = ? THEN ldaReadDate = TODAY - 1.

lcLogFile = fCParamC("DoubleCallLog").
IF lcLogFile = ? THEN lcLogFile = "".
lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                      STRING(MONTH(TODAY),"99") + 
                                      STRING(DAY(TODAY),"99")).

fELog("DAILY","MobCDRDoubleCheckStarted:" + STRING(ldaReadDate,"99-99-9999")).

RUN Mm/mobcdr_double_check.p ("ReadDate",
                           ldaReadDate,
                           ldaReadDate,
                           "",
                           TRUE,
                           FALSE,
                           lcLogFile,
                           0,
                           0,
                           "",
                           OUTPUT liMarked).

fELog("DAILY","MobCDRDoubleCheckStopped:" + 
               STRING(ldaReadDate,"99-99-9999") + ":" + 
               STRING(liMarked)).

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "MobCDR"  
      ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                               STRING(MONTH(TODAY),"99")  +
                               STRING(DAY(TODAY),"99")
      ActionLog.ActionID     = "DoubleCall"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                               MONTH(TODAY)
      ActionLog.ActionDec    = liMarked
      ActionLog.ActionChar   = STRING(liMarked) + 
                               " CDRs read in on " + 
                               STRING(ldaReadDate,"99.99.9999") +
                               " were marked as doubles."
      ActionLog.ActionStatus = 3
      ActionLog.UserCode     = katun
      ActionLog.FromDate     = ldaReadDate
      ActionLog.ToDate       = ldaReadDate.
      ActionLog.ActionTS     = fMakeTS().
END.

QUIT.



