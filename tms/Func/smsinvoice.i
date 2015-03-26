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

{commali.i}
{fcreatereq.i}
{tmsconst.i}

function fSMSInvoiceValidate returns logical
   (  input idaPeriod AS DATE,
      output ocresult as character):

   DEFINE VARIABLE liMonth AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liYear AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldeCurrentMonth AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE ldeNextMonth AS DECIMAL NO-UNDO. 
   
   IF NOT CAN-FIND(FIRST ActionLog WHERE
              ActionLog.Brand = gcBrand AND
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
           MsRequest.Brand = gcBrand AND
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

   def var lireqcreated as int no-undo.

   if not fSMSInvoiceValidate(
      idaPeriod,
      output ocresult) then return 0.

   fcreaterequest(({&REQTYPE_SMS_INVOICE}),
                  idactstamp,
                  iccreator,
                  false,      /* fees */
                  false).    /* send sms */

   assign 
      bcreareq.reqsource = icsource
      bcreareq.reqdtParam1 = idaPeriod
      lireqcreated = bcreareq.msrequest.

   release bcreareq.
   
   return lireqcreated.
     
end function.

&ENDIF
