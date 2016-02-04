{Syst/commali.i}
{Func/fcustbal.i}
{Func/fcustcnt.i}
{Ar/nnpcst.i}

DEF INPUT PARAMETER iiInvNum AS INT NO-UNDO.

DEF BUFFER bInvoice FOR Invoice.


FOR FIRST Invoice EXCLUSIVE-LOCK where
         Invoice.InvNum = iiInvNum,
   FIRST Customer no-lock where
         Customer.CustNum  = Invoice.CustNum:

   /* release events */
   RUN Ar/nnpcst.p (Invoice.InvNum,
                 0,
                 FALSE,
                 INPUT table wMarked).

   FOR EACH SubInvoice OF Invoice EXCLUSIVE-LOCK:

      /* advance payments  */
      IF SubInvoice.AdvPaym NE 0 THEN DO:

         fCustBal(Invoice.CustNum,
                  SubInvoice.CLI,             
                  "AP",
                  -1 * SubInvoice.AdvPaym). 

         /* log-file */
         FOR EACH OPLog EXCLUSIVE-LOCK where
                  OPLog.InvNum    = Invoice.InvNum AND
                  OPLog.SubInvNum = SubInvoice.SubInvNum:

            DELETE OPLog.
         END.
         
         SubInvoice.AdvPaym = 0.
      END.

      /* overpayments  */
      IF SubInvoice.OverPaym NE 0 THEN DO:

         fCustBal(Invoice.CustNum,
                  SubInvoice.CLI,
                  "OP",
                  -1 * SubInvoice.OverPaym). 

         /* log-file */
         FOR EACH OPLog EXCLUSIVE-LOCK where
                  OPLog.InvNum = Invoice.InvNum AND
                  OPLog.SubInvNum = SubInvoice.SubInvNum:

            DELETE OPLog.
         END.   

         SubInvoice.OverPaym = 0.
      END.

      /* minimum consumption */
      FOR FIRST MinConsumption EXCLUSIVE-LOCK WHERE
                MinConsumption.MsSeq  = SubInvoice.MsSeq AND
                MinConsumption.InvNum = Invoice.InvNum:
         DELETE MinConsumption.
      END.
                                     
   END.
 
   FIND FIRST InvGroup where 
              InvGroup.Brand    = Invoice.Brand AND
              InvGroup.InvGroup = Customer.InvGroup
   no-lock no-error.

   IF InvGroup.UpdCustBal THEN DO:

      fCustBal(Invoice.CustNum,
               "", 
               "ARBAL",
               -1 * Invoice.InvAmt). 

      fCustCount(Invoice.CustNum,
                 "UB",
                 Invoice.AmtExclVat). 
   END.

   FIND FIRST bInvoice use-index custnum where
              bInvoice.Brand    = Customer.Brand AND
              bInvoice.CustNum  = Customer.Custnum AND
              bInvoice.CrInvNum = 0 AND
              bInvoice.InvAmt   > 0
   no-lock no-error.
   
   IF AVAIL bInvoice THEN
        fLatestInv(Customer.CustNum,
                   bInvoice.CLI,
                   bInvoice.InvDate). 

   ELSE fCustBal(Customer.CustNum,
                 "",
                 "INVP",
                 0.00). 

END.

RETURN "".


