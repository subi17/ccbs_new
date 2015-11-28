/* ----------------------------------------------------------------------
  MODULE .......: dms_doc_provided.p
  TASK .........: Generate messages if customer response is not received in
                  required tipe period.
  APPLICATION ..: tms
  AUTHOR .......: ilkkasav
  CREATED ......: 28.11.15
  Version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
Katun = "Cron".
{tmsconst.i}
{timestamp.i}
{cparam2.i}
{dms.i}

DEF VAR ldaReadDate       AS DATE NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile1        AS CHAR NO-UNDO.
DEF VAR ldStartTS         AS DECIMAL NO-UNDO.
DEF VAR ldEndTS           AS DECIMAL NO-UNDO.
DEF VAR lcMSg             AS CHAR NO-UNDO.
DEF STREAM sLogFile.


FUNCTION fGetDateRange RETURNS CHAR
   (iiDays AS INT,
    OUTPUT odStart AS DECIMAL,
    OUTPUT odEnd AS DECIMAL):
   DEF VAR ldNow AS DECIMAL.
   ldNow = DECIMAL(INT(fMakeTS())).
   odStart = fOffset(ldNow, -24 * (iiDays + 1)).
   odEnd = fOffset(ldNow, -24 * (iiDays )).

END.

/*Is feature active:*/
IF fDMSOnOff() NE TRUE THEN RETURN.

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

/*Dir and file definition*/
ASSIGN
       ldaReadDate  = TODAY        
       lcLogDir     = fCParam("DMS","TMS_to_DMS_LogDir")

       lcLogFile1    = lcLogDir + "tms_no_response_notif_" +
                      STRING(YEAR(ldaReadDate)) +
                      STRING(MONTH(ldaReadDate),"99") +
                      STRING(DAY(ldaReadDate),"99") + ".log".



OUTPUT STREAM sLogFile TO VALUE(lcLogFile1) APPEND.
fLogLine("","DMS Reminder creation starts " + fTS2HMS(fMakeTS())).

/*define time range for getting the requested entries*/
/*fGetDateRange(3, ldStartTS, ldEndTS).
FOR EACH DMS NO-LOCK WHERE
      /*   DMS.DMSStatusTS  > ldStartTS AND*/ 
        /* DMS.StatusTS < ldEndTS AND*/
         DMS.HostTable EQ {&DMS_TABLE_ORDER}:
   lcErr = fSendChangeInformation(DMS.StatusCode, DMS.HostID, "", lcMsg).

   fLogMsg("Msg : " + lcMsg + " #Status: " + lcErr).
ENa */.
fLogLine("","DMS Reminder creation ends " + fTS2HMS(fMAkeTS())).

OUTPUT STREAM sLogFile CLOSE.







