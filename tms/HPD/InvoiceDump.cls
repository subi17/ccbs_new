/*------------------------------------------------------------------------
    File        : InvoiceDump
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:19:10 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/
{tmsconst.i}
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

   DEFINE PRIVATE TEMP-TABLE ttBillItem NO-UNDO
      FIELD BillCode AS CHARACTER
      INDEX BillCode IS PRIMARY UNIQUE BillCode
      .

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

      EMPTY TEMP-TABLE ttBillItem.
      
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


   METHOD PRIVATE VOID mPopulateBillItem():

      FOR EACH BillItem FIELDS (Brand BIGroup BillCode) NO-LOCK WHERE
         BillItem.Brand   = "1"   AND
         BillItem.BIGroup = "33":

         CREATE ttBillItem.
         ttBillItem.BillCode = BillItem.BillCode.
      END.

   END METHOD.

   METHOD PRIVATE CHAR mGetQ25ExludeRows (INPUT iiInvNum AS INT,
                                          INPUT iiSubInvNum AS INT):
      DEF VAR lcExcludedRows AS CHAR NO-UNDO.
      DEF VAR ldeQ25DiscAmt  AS DEC  NO-UNDO.

      ldeQ25DiscAmt = 0.

      FOR EACH InvRow NO-LOCK WHERE
               InvRow.InvNum = iiInvNum AND
               InvRow.SubInvNum = iiSubInvNum AND
               InvRow.VatPerc = 0 AND
               LOOKUP(InvRow.BillCode,
                      {&Q25_RVTERM_RENEWAL_DISCOUNTS}) > 0:
         ASSIGN
            ldeQ25DiscAmt = ldeQ25DiscAmt + InvRow.Amt
            lcExcludedRows = lcExcludedRows + "," + STRING(ROWID(InvRow)).
      END.

      lcExcludedRows = LEFT-TRIM(lcExcludedRows,",").

      FOR EACH InvRow NO-LOCK WHERE
               InvRow.Invnum = iiInvNum AND
               InvRow.SubInvNum = iiSubInvNum AND
               InvRow.VatPerc = 0 AND
               InvRow.Amt EQ (ldeQ25DiscAmt * -1) AND
               LOOKUP(InvRow.BillCode, {&TF_RVTERM_BILLCODES}) > 0:
         lcExcludedRows = lcExcludedRows + "," + STRING(ROWID(InvRow)).
         LEAVE.
      END.
      IF NOT AVAIL InvRow THEN lcExcludedRows = "".

      RETURN lcExcludedRows.
   END METHOD.   

   METHOD PRIVATE VOID mProcessTimeBased
      (INPUT objDumpTB AS CLASS DumpTimeBased):
         
      DEFINE VARIABLE ldaFromDate      AS DATE NO-UNDO.
      DEFINE VARIABLE ldaToDate        AS DATE NO-UNDO.
      DEFINE VARIABLE ldaISTCDate      AS DATE NO-UNDO.
      DEFINE VARIABLE ldaMaxInvRowDate AS DATE NO-UNDO.
      DEFINE VARIABLE ldeQ25DiscAmt    AS DEC  NO-UNDO.
      DEFINE VARIABLE lcExcludedRows   AS CHAR NO-UNDO.

      ASSIGN
         ldaFromDate = HPDTime:mMake2Date(objDumpTB:ldeFromTS)
         ldaToDate   = HPDTime:mMake2Date(objDumpTB:ldeToTS)
         .

      mPopulateBillItem().

      FOR
         EACH lbInvoice NO-LOCK USE-INDEX InvType WHERE
            lbInvoice.Brand     = "1"                 AND
            lbInvoice.InvType   = 1                   AND
            lbInvoice.InvDate  >= ldaFromDate         AND
            lbInvoice.InvDate   < ldaToDate
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
         
         FOR EACH lbSubInvoice NO-LOCK WHERE
            lbSubInvoice.InvNum = lbInvoice.InvNum:
            
            ASSIGN
               ldaMaxInvRowDate  = 1/1/2000
               ldeInstallmentFee = 0
               ldaISTCDate       = mGetISTCDate(lbSubInvoice.MsSeq,
                                                lbInvoice.Custnum,
                                                lbInvoice.Todate).

            IF CAN-FIND(lbInvRow NO-LOCK WHERE 
                        lbInvRow.InvNum EQ lbSubInvoice.InvNum AND
                        lbInvRow.SubInvNum EQ lbSubInvoice.SubInvNum AND
                        lbInvRow.BillCode EQ "RVTERMDTEQ25" AND
                        lbInvRow.VatPerc EQ 0) THEN DO:
               lcExcludedRows = mGetQ25ExludeRows(lbSubInvoice.InvNum,
                                                  lbSubInvoice.SubInvNum).
            END.

            FOR EACH lbInvRow NO-LOCK WHERE
               lbInvRow.InvNum    = lbInvoice.InvNum        AND
               lbInvRow.SubInvNum = lbSubInvoice.SubInvNum:

               IF CAN-FIND(ttBillItem NO-LOCK WHERE 
                           ttBillItem.BillCode = lbInvRow.BillCode) THEN DO:
                  IF lcExcludedRows > "" AND 
                     LOOKUP(STRING((ROWID(lbInvRow))),lcExcludedRows) > 0 THEN 
                     NEXT.
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
