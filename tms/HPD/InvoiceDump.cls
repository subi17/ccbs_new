/*------------------------------------------------------------------------
    File        : InvoiceDump
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:19:10 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING HPD.InvoiceCommon.
USING HPD.DumpFile.
USING HPD.DumpTimeBased.
USING HPD.DumpBase.
USING HPD.HPDTime.
USING HPD.HPDCommon.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.InvoiceDump FINAL INHERITS InvoiceCommon:

   DEFINE PRIVATE VARIABLE objDumpInvRow     AS CLASS DumpFile NO-UNDO.
   DEFINE PRIVATE VARIABLE objDumpSubInvoice AS CLASS DumpFile NO-UNDO.  
  
   DEFINE PRIVATE BUFFER lbInvRow     FOR InvRow.

   CONSTRUCTOR PUBLIC InvoiceDump
      (iiDumpID AS INTEGER):
      
      lhMainTable = BUFFER lbInvoice:HANDLE.
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).      
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
               
   END CONSTRUCTOR.   
   
   
   CONSTRUCTOR PUBLIC InvoiceDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
      
      objDumpBase = NEW DumpTimeBased(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbInvoice:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      objDumpSubInvoice = NEW DumpFile(HPDCommon:mGetDumpID("HPD_SubInvoice"), THIS-OBJECT).
      objDumpSubInvoice:mUseTable(BUFFER lbSubInvoice:HANDLE).
      objDumpSubInvoice:mUseTable(BUFFER lbInvoice:HANDLE).
      objDumpSubInvoice:mUseTable(BUFFER lbMsOwner:HANDLE).
      objDumpSubInvoice:mVerifyDumpFileData().

      objDumpInvRow     = NEW DumpFile(HPDCommon:mGetDumpID("HPD_InvRow"), THIS-OBJECT).
      objDumpInvRow:mUseTable(BUFFER lbInvRow:HANDLE).
      objDumpInvRow:mUseTable(BUFFER lbInvoice:HANDLE).
      objDumpInvRow:mUseTable(BUFFER lbSubInvoice:HANDLE).
      objDumpInvRow:mVerifyDumpFileData().
      
      mProcessTimeBased(INPUT CAST(objDumpBase, DumpTimeBased)).

      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.
   
   
   DESTRUCTOR PUBLIC InvoiceDump( ):
     
      IF VALID-OBJECT(objDumpInvRow)
      THEN DELETE OBJECT objDumpInvRow.

      IF VALID-OBJECT(objDumpSubInvoice)
      THEN DELETE OBJECT objDumpSubInvoice.
      
   END DESTRUCTOR.   
   
   
   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "SubInvoiceKey"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN {HPD/keyvalue.i lbSubInvoice . objDumpSubInvoice:lcKeyDelimiter InvNum SubInvNum}.
         WHEN "InvRowKey"
         THEN IF ilCheckOnly
              THEN RETURN "OK".            
              ELSE RETURN {HPD/keyvalue.i lbInvRow . objDumpInvRow:lcKeyDelimiter InvNum SubInvNum InvRowNum}. 
         /* Because HPD has integer format, we can't use decimal
            and need to convert to integer.
            This is a bad solution and we should use decimal. */
         WHEN "Qty"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(INTEGER(lbInvRow.Qty)).
         WHEN "Amt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(ROUND(lbInvRow.Amt,2)).
         WHEN "VATAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(HPDCommon:mVATAmt(lbInvRow.Amt, lbInvRow.VATPerc)).
         WHEN "TotalAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(ROUND(lbInvRow.Amt + HPDCommon:mVATAmt(lbInvRow.Amt, lbInvRow.VATPerc),2)).
         OTHERWISE RETURN SUPER:mSpecialHandling(icField, ilCheckOnly).
      END CASE.
   
   END METHOD.


   METHOD PRIVATE VOID mProcessTimeBased
      (INPUT objDumpTB AS CLASS DumpTimeBased):
         
      DEFINE VARIABLE ldaFromDate      AS DATE NO-UNDO.
      DEFINE VARIABLE ldaISTCDate      AS DATE NO-UNDO.
      DEFINE VARIABLE ldaMaxInvRowDate AS DATE NO-UNDO.

      ldaFromDate = HPDTime:mMake2Date(objDumpTB:ldeFromTS).

      FOR
         EACH lbInvoice NO-LOCK USE-INDEX InvType WHERE
            lbInvoice.Brand     = "1"                 AND
            lbInvoice.InvType   = 1                   AND
            lbInvoice.InvDate  >= ldaFromDate         AND
            lbInvoice.ChgStamp >= objDumpTB:ldeFromTS AND
            lbInvoice.ChgStamp  < objDumpTB:ldeToTS,
         FIRST Customer FIELDS (CustNum) NO-LOCK WHERE
            Customer.CustNum = lbInvoice.CustNum
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.
         
         IF lbInvoice.InvCfg[1]
         THEN NEXT.         
         
         liEvents = liEvents + 1.

         /* Lets write a line for invoice to csv */
         objDumpTB:mWriteALine().
         
         FOR EACH lbSubInvoice OF lbInvoice NO-LOCK:
            
            ASSIGN
               ldaMaxInvRowDate  = 1/1/2000
               ldeInstallmentFee = 0
               ldaISTCDate       = mGetISTCDate(lbSubInvoice.MsSeq,
                                                lbInvoice.Custnum,
                                                lbInvoice.Todate).

            FOR EACH lbInvRow NO-LOCK WHERE
               lbInvRow.InvNum = lbInvoice.InvNum           AND 
               lbInvRow.SubInvNum = lbSubInvoice.SubInvNum:

               FOR FIRST BillItem FIELDS (Brand BillCode BIGroup) NO-LOCK WHERE
                  BillItem.Brand    = "1"               AND
                  BillItem.BillCode = lbInvRow.BillCode AND
                  BillItem.BIGroup = "33":

                  ldeInstallmentFee = ldeInstallmentFee + lbInvRow.Amt.
               END.

               IF ldaISTCDate NE ? AND
                  ldaMaxInvRowDate < lbInvRow.ToDate
               THEN ldaMaxInvRowDate = lbInvRow.Todate.

              /* Lets write a line for invrow to csv */
               objDumpInvRow:mWriteALine().
            END.

            mFindMsOwner(ldaISTCDate,ldaMaxInvRowDate).

            /* Lets write a line for subinvoice to csv */
            objDumpSubInvoice:mWriteALine().

         END.

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "Invoice Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.       
      END.
      
      IF NOT SESSION:BATCH THEN 
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.    

END CLASS.