/* ----------------------------------------------------------------------
  MODULE .......: duplicateinvoice.p 
  TASK .........: Handles duplicate invoices requests (on one run)
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 15.12.09
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Inv/printdoc1tt.i}
{Func/cparam2.i}
{Func/duplicate_invoice.i}
{Syst/tmsconst.i}
{Func/msreqfunc.i}
{Func/multitenantfunc.i}

DEF VAR liInvCount AS INTEGER NO-UNDO.
DEF VAR liPrinted AS INT NO-UNDO. 
DEF VAR lcSpoolDir AS CHARACTER NO-UNDO.
DEF VAR lcInvoiceDir AS CHARACTER NO-UNDO. 
DEF VAR lcFileCall AS CHARACTER NO-UNDO. 
DEF VAR lcFile AS CHARACTER NO-UNDO. 
DEF VAR ldeNow AS DECIMAL NO-UNDO. 
DEF VAR lcError AS CHARACTER NO-UNDO. 
   
lcSpoolDir = fCParam("WholeInvDir", "tmp").
lcInvoiceDir = fCParam("WholeInvDir", "invoices").

IF lcSpoolDir = ? OR TRIM(lcSpoolDir) = "" OR
   lcInvoiceDir = ? OR TRIM(lcInvoiceDir) = "" THEN DO:
   MESSAGE "Directories not configured".
   RETURN.
END.

ldeNow = fMakeTS().
      
DUP_REQ_LOOP:      
FOR EACH MsRequest WHERE
   MsRequest.Brand = gcBrand AND
   MsRequest.ReqType = {&REQTYPE_DUPLICATE_INVOICE} AND
   MsRequest.ReqStatus = {&REQUEST_STATUS_NEW} AND 
   MsRequest.ActStamp <= ldeNow NO-LOCK
   BY MsRequest.MsRequest:

   fReqStatus({&REQUEST_STATUS_UNDER_WORK},"").
     
   lcError = "".

   FIND Invoice WHERE
        Invoice.InvNum = MsRequest.ReqIParam1 NO-LOCK NO-ERROR.
   IF NOT AVAIL Invoice THEN DO:
      lcError = "Invoice not found".
   END.

   IF lcError = "" THEN fDuplicateInvoiceValidate(
      BUFFER Invoice,
      OUTPUT lcError).

   IF lcError NE "" AND lcError NE {&MSG_ONG_REQUEST} THEN DO:
      fReqError(lcError).
      NEXT DUP_REQ_LOOP.
   END.

   FIND ttInvoice WHERE ttInvoice.InvNum = Invoice.InvNum NO-LOCK NO-ERROR.
   IF AVAIL ttInvoice THEN DO:
      fReqError("More than one duplicate requests for the same invoice").
      NEXT DUP_REQ_LOOP.
   END.

   CREATE ttInvoice.
   ASSIGN ttInvoice.InvNum = Invoice.InvNum
          ttInvoice.MsRequest = MsRequest.MsRequest
          liInvCount = liInvCount + 1. 

   FIND Customer WHERE
        Customer.Custnum = Invoice.Custnum NO-LOCK.
   
   /* not used at the moment with Yoigo */
   IF Customer.IDelName > "" THEN 
      ttInvoice.ZipCode = Customer.IDelZipCode.
   ELSE 
      ttInvoice.ZipCode = Customer.ZipCode.

END.
               
IF NOT CAN-FIND(FIRST ttInvoice) THEN RETURN.

lcFile = lcSpoolDir + 
         CAPS(fgetBrandNamebyTenantId(TENANT-ID(LDBNAME(1)))) +
         "_NewDup" + STRING(YEAR(TODAY),"9999") +
   STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + ".txt".
lcFileCall = lcInvoiceDir + "*" + lcFile.

RUN Inv/invoice_xml.p (INPUT-OUTPUT TABLE ttInvoice,
                   TODAY, /* not used */
                   liInvCount,
                   FALSE, /* separate files */
                   lcFileCall,
                   "QVANTEL", /* print house */
                   INTEGER(FALSE), /* tar file */
                   TRUE, /* log errors to db */
                   0, /* func.run process id */
                   0, /* func.run update interval */
                   OUTPUT liPrinted).

/* Mark request statuses */
FOR EACH ttInvoice NO-LOCK:

   FIND MsRequest WHERE
        MsRequest.MsRequest = ttInvoice.MsRequest
   NO-LOCK NO-ERROR.
   
   IF NOT AVAIL MsRequest THEN NEXT.

   CASE ttInvoice.Printed:
      WHEN 0 THEN fReqError("Printing skipped for unknown reason").
      WHEN 1 THEN fReqStatus({&REQUEST_STATUS_DONE},"").
      WHEN 2 THEN fReqError(ttInvoice.ErrMsg).
      OTHERWISE fReqError("Unknown printing status").
   END.
        
END.

EMPTY TEMP-TABLE ttInvoice. 
