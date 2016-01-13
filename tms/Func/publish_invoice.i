/* ----------------------------------------------------------------------
  module .......: publish_invoice.i
  task .........: Creates Publish Invoice request functions (YDR-104)
  application ..: tms
  author .......: subhash sanjeevi
  created ......: 06.08.2015
  version ......: xfera
----------------------------------------------------------------------- */
&IF "{&PUBLISH_INVOICE_I}" NE "YES"
&THEN
&GLOBAL-DEFINE PUBLISH_INVOICE_I YES

{commali.i}
{fcreatereq.i}
{tmsconst.i}

function fPublishInvoiceValidate returns logical
   (INPUT  idaPeriod AS DATE,
    OUTPUT ocresult  AS CHAR):

   DEFINE VARIABLE liMonth         AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liYear          AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldeCurrentMonth AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE ldeNextMonth    AS DECIMAL NO-UNDO. 
   
   IF CAN-FIND(FIRST ActionLog WHERE
                     ActionLog.Brand        = gcBrand    AND
                     ActionLog.TableName    = "Invoice"  AND
                     ActionLog.ActionID     = "WebDisp"  AND 
                     ActionLog.ActionPeriod = YEAR(idaPeriod) * 100 + MONTH(idaPeriod)
                     NO-LOCK) THEN DO:
      ocResult = "Invoices are already Published for current month".
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

      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.Brand    = gcBrand                      AND
                 MsRequest.ReqType  = ({&REQTYPE_PUBLISH_INVOICE}) AND
                 MsRequest.ActStamp > ldeCurrentMonth              AND
                 MsRequest.ActStamp < ldeNextMonth                 AND
           LOOKUP(STRING(MsRequest.ReqStatus),"0,1,2,3") > 0       NO-ERROR.

      IF AVAIL MsRequest THEN DO:
         ocResult = "Publish invoice request for current month is ongoing or done".
      END.
   END.

   RETURN ocresult EQ "". 

end function. 

function fPublishInvoiceRequest returns integer
   (INPUT  idactstamp AS DEC,    /* when request should be handled */
    INPUT  idaPeriod  AS DATE,
    INPUT  iccreator  AS CHAR,
    INPUT  icsource   AS CHAR,
    OUTPUT ocresult   AS CHAR).

   DEF VAR lireqcreated AS INT NO-UNDO. 

   IF NOT fPublishInvoiceValidate(
      idaPeriod,
      OUTPUT ocresult) THEN RETURN 0.

   fcreaterequest(({&REQTYPE_PUBLISH_INVOICE}),
                  idactstamp,
                  iccreator,
                  false,      /* fees */
                  false).    /* send sms */

   ASSIGN 
      bcreareq.reqsource   = icsource
      bcreareq.reqdtParam1 = idaPeriod
      lireqcreated         = bcreareq.msrequest.

   RELEASE bcreareq.
   
   RETURN lireqcreated.
     
end function.

&ENDIF

