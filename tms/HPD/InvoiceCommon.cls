USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.HPDTime.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.InvoiceCommon INHERITS DumpHandler:

   DEFINE PROTECTED BUFFER lbInvoice    FOR Invoice.
   DEFINE PROTECTED BUFFER lbSubInvoice FOR SubInvoice.
   DEFINE PROTECTED BUFFER lbMsOwner    FOR MsOwner.

   DEFINE PROTECTED VARIABLE ldeInstallmentFee AS DECIMAL NO-UNDO.


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "InstallmentFee"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(ldeInstallmentFee,"->>>>>>>>>>>>>>>>>>>>>>9.9<<<<<<<<<").
         OTHERWISE RETURN "".
      END CASE.
   
   END METHOD.


   METHOD PROTECTED DATE mGetISTCDate
      (INPUT iiMsSeq   AS INTEGER,
       INPUT iiCustnum AS INTEGER,
       INPUT idaPeriod AS DATE):

      DEFINE VARIABLE ldeFromTs   AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ldeEndTs    AS DECIMAL NO-UNDO.

      ASSIGN
         ldeFromTs = YEAR(idaPeriod) * 10000 + MONTH(idaPeriod) * 100 + 1
         ldeEndTs  = HPDTime:mMake2DT(HPDTime:mLastDayOfMonth(idaPeriod),86399).

      IF iiCustnum > 0 THEN
      FIND FIRST MsOwner NO-LOCK WHERE
                 MsOwner.MsSeq    = iiMsSeq   AND
                 MsOwner.TsBeg   >= ldeFromTs AND
                 MsOwner.TsBeg   <= ldeEndTs  AND
                 MsOwner.Custnum  = iiCustnum AND
                 MsOwner.CLIEvent BEGINS "iS" NO-ERROR.
      ELSE
      FIND FIRST MsOwner NO-LOCK WHERE
                 MsOwner.MsSeq    = iiMsSeq   AND
                 MsOwner.TsBeg   >= ldeFromTs AND
                 MsOwner.TsBeg   <= ldeEndTs  AND
                 MsOwner.CLIEvent BEGINS "iS" NO-ERROR.
   
      IF NOT AVAILABLE MsOwner
      THEN RETURN ?.

      RETURN HPDTime:mMake2Date(MsOwner.TsBegin).

   END METHOD.


   METHOD PROTECTED VOID mFindMsOwner
      (idaISTCDate      AS DATE,
       idaMaxInvRowDate AS DATE):

      RELEASE lbMSOwner.

      IF idaISTCDate NE ? AND 
         idaMaxInvRowDate < idaISTCDate
      THEN
      FIND FIRST lbMsOwner NO-LOCK USE-INDEX MsSeq WHERE 
         lbMsOwner.MsSeq   = lbSubInvoice.MsSeq AND
         lbMsOwner.TsBeg   < HPDTime:mMake2Dt(idaISTCDate,0)  AND
         lbMsOwner.TsEnd  >= HPDTime:mMake2Dt(lbInvoice.FromDate,0) AND
         lbMsOwner.PayType = FALSE AND
         lbMsOwner.Custnum = lbInvoice.Custnum
      NO-ERROR.
      
      IF NOT AVAILABLE lbMsOwner
      THEN
      FIND FIRST lbMsOwner NO-LOCK USE-INDEX MsSeq WHERE 
         lbMsOwner.MsSeq   = lbSubInvoice.MsSeq                     AND
         lbMsOwner.TsBeg  <= HPDTime:mMake2Dt(lbInvoice.ToDate,0)   AND
         lbMsOwner.TsEnd  >= HPDTime:mMake2Dt(lbInvoice.FromDate,0) AND
         lbMsOwner.PayType = FALSE AND
         lbMsOwner.Custnum = lbInvoice.Custnum 
      NO-ERROR.
         
      IF NOT AVAILABLE lbMsOwner
      THEN
      FIND FIRST lbMsOwner NO-LOCK USE-INDEX MsSeq WHERE 
         lbMsOwner.MsSeq   = lbSubInvoice.MsSeq                     AND
         lbMsOwner.InvCust = lbInvoice.Custnum 
      NO-ERROR.
   
   END METHOD.

END CLASS.