/* ----------------------------------------------------------------------
  module .......: sms_invoice.i
  task .........: SMS Invoice request functions (YDR-104)
  application ..: tms
  author .......: anttis 
  created ......: 14.12.09
  version ......: xfera
----------------------------------------------------------------------- */
&IF "{&SMS_INVOICE_I}" NE "YES"
&THEN
&GLOBAL-DEFINE SMS_INVOICE_I YES

{Syst/commali.i}
{Func/fcreatereq.i}
{Syst/tmsconst.i}
{Func/cparam2.i}

function fSMSInvoiceValidate returns logical
   (  input idaPeriod AS DATE,
      output ocresult as character):

   DEFINE VARIABLE liMonth AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liYear AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldeCurrentMonth AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE ldeNextMonth AS DECIMAL NO-UNDO. 
   
   IF NOT CAN-FIND(FIRST ActionLog WHERE
              ActionLog.Brand = Syst.Var:gcBrand AND
              ActionLog.ActionID = "DDFILES" AND
              ActionLog.ActionPeriod = YEAR(idaPeriod) * 100 + MONTH(idaPeriod)
              NO-LOCK) THEN DO:
      ocResult = "Direct debit files for current month are not created".
   END.
   ELSE DO:
      
      liMonth = MONTH(idaPeriod) + 1.
      liYear = YEAR(idaPeriod).

      IF liMonth = 13 THEN DO:
         liMonth = 1.
         liYear = liYear + 1.
      END.

      ldeCurrentMonth = YEAR(idaPeriod) * 10000 + MONTH(idaPeriod) * 100 + 1.
      ldeNextMonth = liYear * 10000 + liMonth * 100 + 1.

      FIND FIRST MsRequest WHERE
           MsRequest.Brand = Syst.Var:gcBrand AND
           MsRequest.ReqType = ({&REQTYPE_SMS_INVOICE}) AND
           MsRequest.ActStamp > ldeCurrentMonth AND
           MsRequest.ActStamp < ldeNextMonth AND
           LOOKUP(STRING(MsRequest.ReqStatus),"0,1,2,3") > 0 NO-LOCK NO-ERROR.

      IF AVAIL MsRequest THEN DO:
         ocResult = "SMS invoice request for current month is ongoing or done".
      END.
   END.

   return ocresult eq "". 

end function. 

function fsmsinvoicerequest returns integer
   (input  idactstamp    as dec,    /* when request should be handled */
    input  idaPeriod     as date,
    input  iccreator     as char,
    input  icsource      as char,
    output ocresult      as char).

def var lireqcreated as int  no-undo.
DEF VAR lButtonSeconds     AS DECIMAL   NO-UNDO.
DEF VAR lButtonDate   AS DATE      NO-UNDO.
DEF VAR lEndSeconds   AS INTEGER   NO-UNDO.
DEF VAR lIniSeconds   AS INTEGER   NO-UNDO.
DEF VAR lcSMSSchedule AS CHARACTER NO-UNDO.

   /* Time of request */
   Func.Common:mSplitTS(idactstamp, lButtonDate, lButtonSeconds).

   /* ie. "32400-79200" Send between 9:00-22:00 YOT-4130 */
   lcSMSSchedule = fCParamC("SMSSchedule").
   lIniSeconds = INTEGER(ENTRY(1,lcSMSSchedule,"-")) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN lIniSeconds = 0.
   lEndSeconds = INTEGER(ENTRY(2,lcSMSSchedule,"-")) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN lEndSeconds = 0.

   IF lIniSeconds <= 0 THEN lIniSeconds = 1.
   IF lIniSeconds > 86399 THEN lIniSeconds = 86399. /* 23:59:59 */
   
   IF lEndSeconds <= 0 THEN lEndSeconds = 1.
   IF lEndSeconds > 86399 THEN lEndSeconds = 86399. /* 23:59:59 */
   
   IF lIniSeconds >= lEndSeconds THEN
   ASSIGN /* 9:00-22:00 */
      lIniSeconds = 32400
      lEndSeconds = 86399.

   /* If is too late, schedule to start next morning */
   IF (lButtonSeconds > lEndSeconds) THEN
   DO:
      lButtonDate = ADD-INTERVAL (lButtonDate, 1, "days").
      idactstamp = Func.Common:mHMS2TS(lButtonDate, STRING(lIniSeconds,"hh:mm:ss")) .
   END.
   ELSE
   /* If is too early, schedule to start when window opens */
   IF (lButtonSeconds < lIniSeconds) THEN
   DO:
      idactstamp = Func.Common:mHMS2TS(lButtonDate, STRING(lIniSeconds,"hh:mm:ss")) .
   END.
   
   if not fSMSInvoiceValidate(
      idaPeriod,
      output ocresult) then return 0. 

   fcreaterequest(({&REQTYPE_SMS_INVOICE}),
                  idactstamp,
                  iccreator,
                  false,     /* fees */
                  false).    /* send sms */

   assign 
      bcreareq.reqsource = icsource
      bcreareq.reqdtParam1 = idaPeriod
      lireqcreated = bcreareq.msrequest.

   release bcreareq.
   
   return lireqcreated.
     
end function.

&ENDIF
