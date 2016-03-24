USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.HPDCommon.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.InvRowDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbInvRow     FOR InvRow.
   DEFINE PRIVATE BUFFER lbInvoice    FOR Invoice.
   DEFINE PRIVATE BUFFER lbSubInvoice FOR SubInvoice.


   CONSTRUCTOR PUBLIC InvRowDump
      (iiDumpID AS INTEGER):

      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbInvRow:HANDLE).
      objDumpBase:mUseTable(BUFFER lbInvoice:HANDLE).
      objDumpBase:mUseTable(BUFFER lbSubInvoice:HANDLE).
      objDumpBase:mVerifyDumpFileData().

   END CONSTRUCTOR.


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "InvRowKey"
         THEN IF ilCheckOnly
              THEN RETURN "OK".            
              ELSE RETURN {HPD/keyvalue.i lbInvRow . objDumpBase:lcKeyDelimiter InvNum SubInvNum InvRowNum}.
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
         OTHERWISE RETURN "".          
         
      END CASE.
   
   END METHOD.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      FIND lbInvRow NO-LOCK WHERE
         ROWID(lbInvRow) = TO-ROWID(icRowID)
      NO-ERROR.

      IF AVAILABLE lbInvRow
      THEN DO:

         FIND lbInvoice NO-LOCK WHERE
            lbInvoice.InvNum = lbInvRow.InvNum
         NO-ERROR.

         FIND lbSubInvoice NO-LOCK WHERE
            lbSubInvoice.InvNum    = lbInvRow.InvNum AND
            lbSubInvoice.SubInvNum = lbInvRow.SubInvNum
         NO-ERROR.

         RETURN objDumpBase:mGetData().

      END.

      RETURN "".

   END METHOD.

END CLASS.