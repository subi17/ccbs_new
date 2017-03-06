/* fcreditvalid.i         13.10.09/rafaeldv 

   validate some bussines logic to when 
   credits notes are created 
*/
&IF "{&FCREDITVALID_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE FCREDITVALID_I YES
  
{Func/finvbal.i}
{Func/fparse.i}

FUNCTION fCheckCreditNoteRequest RETURNS CHARACTER 
         (INPUT iiCustNum AS INT,
          INPUT iiInvNum AS INT):

    DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO INITIAL "". 
   /* already in process (also status 3 prevents a new request) */
   IF CAN-FIND(FIRST MsRequest USE-INDEX CustNum WHERE
                     MsRequest.Brand      = gcBrand         AND
                     MsRequest.ReqType    = 22              AND
                     MsRequest.CustNum    = iiCustNum AND
                     MsRequest.ReqIParam1 = iiInvNum  AND
                     LOOKUP(STRING(MsRequest.ReqStatus),"0,1,3") > 0)
    THEN 
     lcReturn = "Previous credit note request for this invoice must be handled first.".

    RETURN lcReturn.

END FUNCTION.


FUNCTION fCheckInvoice RETURNS CHARACTER 
         (BUFFER chkBInvoice FOR Invoice,
          INPUT pcSubInvoices AS CHAR,
          INPUT pcInvRowNumList AS CHAR,
          OUTPUT ocCreditMode AS CHAR) :

   DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO INITIAL "".
   DEF VAR i AS INT NO-UNDO. 
   DEF VAR liSubInvNum AS INT NO-UNDO. 
   DEF VAR llNeedtoCredit AS LOG NO-UNDO.

   DEF BUFFER lbSubInvoice FOR SubInvoice.
   DEF BUFFER lbInvRow     FOR InvRow.

   IF LOOKUP(STRING(chkBInvoice.InvType),"0,1,6,7") = 0 THEN DO:
      lcReturn = "Invoice is not creditable. Function not allowed".
      RETURN lcReturn.
   END.
   /* IF this invoice is already credited */
   IF chkBInvoice.CrInvNum > 0 THEN DO:
      IF pcSubInvoices EQ "" THEN DO:
         lcReturn = "Invoice has already been credited. Function not allowed".
         RETURN lcReturn.
      END.

      FOR EACH lbSubInvoice OF chkBInvoice WHERE
               lbSubInvoice.CrInvNum > 0 NO-LOCK:
         IF LOOKUP(STRING(lbSubInvoice.SubInvNum),pcSubInvoices) > 0 THEN DO:
            llNeedtoCredit = FALSE.

            /* Allow SubInvoice again credit, if any InvRow is still   */
            /* pending  regardless of InvRow List is specified or not  */
            FOR EACH lbInvRow OF lbSubInvoice NO-LOCK:
               IF lbInvRow.CreditInvNum > 0 THEN DO:
                  IF LOOKUP(STRING(lbInvRow.InvRowNum),pcInvRowNumList) > 0
                  THEN DO:
                     lcReturn = "InvRow has already been credited. " +
                                "Function not allowed".
                     RETURN lcReturn.
                  END. /* IF LOOKUP(STRING(lbInvRow.InvRowNum) */
               END. /* IF lbInvRow.CreditInvNum > 0 THEN DO: */
               ELSE llNeedtoCredit = TRUE.
            END. /* FOR EACH lbInvRow OF chkBInvoice WHERE */

            IF NOT llNeedtoCredit THEN DO:
               lcReturn = "SubInvoice has already been credited. " +
                          "Function not allowed".
               RETURN lcReturn.
            END. /* IF NOT llNeedtoCredit THEN DO: */
         END. /* IF LOOKUP(STRING(lbSubInvoice.SubInvNum) */
      END. /* FOR EACH lbSubInvoice OF chkBInvoice WHERE */
   END. /* IF chkBInvoice.CrInvNum > 0 THEN DO: */

   IF llNeedtoCredit OR pcInvRowNumList > "" THEN ocCreditMode = "Partial".
   ELSE ocCreditMode = "Full".

 RETURN lcReturn.

END FUNCTION.

FUNCTION fCheckInvoicePayments RETURNS CHARACTER 
         (BUFFER chkBInvoice FOR Invoice) :

   DEFINE VARIABLE ldBal AS DECIMAL NO-UNDO. 

   /* check payments */
   ldBal = fInvBal(BUFFER chkBInvoice,
              TODAY).

   IF ldBal NE chkBInvoice.InvAmt AND
      CAN-FIND(FIRST Payment OF chkBInvoice) AND
        (chkBInvoice.InvType > 1 OR chkBInvoice.ChargeType NE 2) THEN DO:
       RETURN "Invoice has already been " +
                      (IF chkBInvoice.PaymState = 1
                       THEN "partly " ELSE "") + "paid. Function not allowed".
   END.

   RETURN "".
END.

&ENDIF
