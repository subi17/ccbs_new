/* ----------------------------------------------------------------------
  MODULE .......: dms_doc_provided.p
  TASK .........: Generate messages if customer response is not received in
                  required tipe period.
  APPLICATION ..: tms
  AUTHOR .......: ilkkasav
  CREATED ......: 28.11.15
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "Cron".
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/dms.i}

DEF VAR ldaReadDate       AS DATE NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile1        AS CHAR NO-UNDO.
DEF VAR ldStartTS         AS DECIMAL NO-UNDO.
DEF VAR ldEndTS           AS DECIMAL NO-UNDO.
DEF VAR lcMSg             AS CHAR NO-UNDO.
DEF VAR lcErr             AS CHAR NO-UNDO.
DEF VAR liNoDocProvidedPeriod AS INT NO-UNDO.
DEF VAR lcNoDocProvidedStatuses AS CHAR NO-UNDO.
DEF VAR lcActionID        AS CHAR NO-UNDO.
DEF VAR lcTableName       AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS   AS DEC  NO-UNDO.
DEF STREAM sLogFile.

lcTableName = "DMS".
lcActionID = {&DMS_REMINDER_SENDER}.
ldCurrentTimeTS = Func.Common:mMakeTS().

/*iiDays: how many days are between today and the selected date
For example
0 - ysterday*/
FUNCTION fGetDateRange RETURNS CHAR
   (iiDays AS INT,
    OUTPUT odStart AS DECIMAL,
    OUTPUT odEnd AS DECIMAL):
   DEF VAR ldNow AS DECIMAL.
   ldNow = Func.Common:mDate2TS(TODAY).
   odStart = Func.Common:mOffSet(ldNow, -24 * (iiDays + 1)).
   odEnd = Func.Common:mOffSet(ldNow, -24 * (iiDays )).

END.

FUNCTION fLogLine RETURNS LOGICAL
   (icLine AS CHAR,
   icMessage AS CHAR):
   PUT STREAM sLogFile UNFORMATTED
      icLine "#"
      icMessage "#"
      "TMS" SKIP.
END FUNCTION.

FUNCTION fLogMsg RETURNS LOGICAL
   (icMessage AS CHAR):
   PUT STREAM sLogFile UNFORMATTED
      icMessage "#"
      "DMS" SKIP.
END FUNCTION.

/*Is feature active:*/
IF fDMSOnOff() NE TRUE THEN RETURN.

DO TRANS:

   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = Syst.Var:gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      RETURN. /*No reporting in first time.*/
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE Actionlog.
   END.
END.

/*Dir and file definition*/
ASSIGN
       ldaReadDate  = TODAY        
       lcLogDir     = fCParam("DMS","TMS_to_DMS_LogDir")

       lcLogFile1    = lcLogDir + "tms_no_response_notif_" +
                      STRING(YEAR(ldaReadDate)) +
                      STRING(MONTH(ldaReadDate),"99") +
                      STRING(DAY(ldaReadDate),"99") + ".log".


OUTPUT STREAM sLogFile TO VALUE(lcLogFile1) APPEND.
fLogLine("","DMS Reminder creation starts " + Func.Common:mTS2HMS(Func.Common:mMakeTS())).

/*define time range for getting the requested entries*/
liNoDocProvidedPeriod = fCParamI("DMS_doc_no_provided_time").
fGetDateRange(liNoDocProvidedPeriod, ldStartTS, ldEndTS).

lcNoDocProvidedStatuses =  fCParam("DMS","DMS_doc_no_provided_statuses").

FOR EACH DMS NO-LOCK WHERE
         DMS.StatusTS  >= ldStartTS AND
         DMS.StatusTS < ldEndTS AND
         DMS.HostTable EQ {&DMS_HOST_TABLE_ORDER}:

   IF LOOKUP(DMS.StatusCode, lcNoDocProvidedStatuses) > 0 THEN DO:
     /*Pending (A0), Doc errónea status (C)*/
     lcErr = fSendChangeInformation(DMS.StatusCode  + "_by_batch" ,
                                    DMS.HostID, 
                                    "", /*deposit*/
                                    "", /*docListSep*/
                                    "doc_reminder",
                                    lcMsg).
     fLogMsg(lcMsg).
     fLogLine("","Msg sending status " + lcErr).
   END.
END.
     fLogLine("","DMS Reminder creation ends " + Func.Common:mTS2HMS(Func.Common:mMakeTS())).

OUTPUT STREAM sLogFile CLOSE.

DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND 
              ActionLog.ActionStatus NE {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.

