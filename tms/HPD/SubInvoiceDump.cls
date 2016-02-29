/*------------------------------------------------------------------------
    File        : SubInvoiceDump
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:19:10 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING HPD.InvoiceCommon.
USING HPD.DumpBase.
USING HPD.HPDTime.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.SubInvoiceDump FINAL INHERITS InvoiceCommon:
   
   CONSTRUCTOR PUBLIC SubInvoiceDump
      (iiDumpID AS INTEGER):
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).      
      objDumpBase:mUseTable(BUFFER lbSubInvoice:HANDLE).
      objDumpBase:mUseTable(BUFFER lbMsOwner:HANDLE).
      objDumpBase:mVerifyDumpFileData().
               
   END CONSTRUCTOR.
   
   
   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "SubInvoiceKey"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN {HPD/keyvalue.i lbSubInvoice . objDumpBase:lcKeyDelimiter InvNum SubInvNum}.
         OTHERWISE RETURN SUPER:mSpecialHandling(icField, ilCheckOnly).
         
      END CASE.
   
   END METHOD.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      DEFINE VARIABLE ldaISTCDate      AS DATE NO-UNDO.
      DEFINE VARIABLE ldaMaxInvRowDate AS DATE NO-UNDO.

      FIND lbSubInvoice NO-LOCK WHERE
         ROWID(lbSubInvoice) = TO-ROWID(icRowID)
      NO-ERROR.
      
      IF AVAILABLE lbSubInvoice
      THEN DO:

         FIND lbInvoice NO-LOCK WHERE
            lbInvoice.InvNum = lbSubInvoice.InvNum
         NO-ERROR.
         
         ASSIGN
            ldaMaxInvRowDate  = 1/1/2000
            ldeInstallmentFee = 0
            ldaISTCDate       = mGetISTCDate(lbSubInvoice.MsSeq,
                                             lbInvoice.Custnum,
                                             lbInvoice.Todate).         

         FOR
            EACH InvRow FIELDS (InvNum SubInvNum BillCode ToDate) NO-LOCK WHERE
               InvRow.InvNum    = lbInvoice.InvNum        AND 
               InvRow.SubInvNum = lbSubInvoice.SubInvNum,
            FIRST BillItem FIELDS (Brand BillCode BIGroup) NO-LOCK WHERE
               BillItem.Brand    = "1"             AND
               BillItem.BillCode = InvRow.BillCode AND
               BillItem.BIGroup = "33":

            ldeInstallmentFee = ldeInstallmentFee + InvRow.Amt.

            IF ldaISTCDate NE ? AND
               ldaMaxInvRowDate < InvRow.ToDate
            THEN ldaMaxInvRowDate = InvRow.Todate.
         
         END.

         mFindMsOwner(ldaISTCDate,ldaMaxInvRowDate).

         RETURN objDumpBase:mGetData().
      
      END.
            
      RETURN "". 
         
   END METHOD.    

END CLASS.