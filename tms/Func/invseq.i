/* 

29.11.2000: used FOR nnlapo / nncimu TO CREATE InvSeq FOR 
            removed & credited invoices

changes:    20.09.07/aam msseq based invseq 
*/


FUNCTION fNewInvSeq RETURNS INTEGER
   (iiAgrCust AS INT,
    iiInvCust AS INT,
    iiMsSeq   AS INT,
    idtDate   AS DATE,
    iiCDRType AS INT):     /* 0=mobcdr, 1=prepcdr */

   DEF VAR ldtFromDate AS DATE NO-UNDO.
   DEF VAR ldtToDate   AS DATE NO-UNDO.
   
   ASSIGN
      ldtFromDate = DATE(MONTH(idtDate),1,YEAR(idtDate))
      ldtToDate   = ldtFromDate + 32
      ldtToDate   = DATE(MONTH(ldtToDate),1,YEAR(ldtToDate)) - 1.

   /* postpaid */
   IF iiCDRType = 0 THEN DO:
      FIND FIRST InvSeq NO-LOCK WHERE
                 InvSeq.MsSeq   = iiMsSeq   AND
                 InvSeq.CustNum = iiInvCust AND
                 InvSeq.Billed  = FALSE     AND
                 InvSeq.ToDate  = ldtToDate AND
                 InvSeq.AgrCust = iiAgrCust NO-ERROR.
              
      IF NOT AVAILABLE InvSeq THEN DO:
        CREATE InvSeq.
        ASSIGN
           InvSeq.MsSeq    = iiMsSeq
           InvSeq.CustNum  = iiInvCust
           InvSeq.AgrCust  = iiAgrCust
           InvSeq.FromDate = ldtFromDate
           InvSeq.ToDate   = ldtToDate
           InvSeq.InvSeq   = NEXT-VALUE(InvSeq)
           InvSeq.Billed   = FALSE.
      END.

      RETURN InvSeq.InvSeq.
   
   END.
   
   /* prepaid */
   ELSE DO:
   
      FIND FIRST PPInvSeq NO-LOCK WHERE
                 PPInvSeq.MsSeq   = iiMsSeq   AND
                 PPInvSeq.CustNum = iiInvCust AND
                 PPInvSeq.Billed  = FALSE     AND
                 PPInvSeq.ToDate  = ldtToDate NO-ERROR.
              
      IF NOT AVAILABLE PPInvSeq THEN DO:
        CREATE PPInvSeq.
        ASSIGN
           PPInvSeq.MsSeq    = iiMsSeq
           PPInvSeq.CustNum  = iiInvCust
           PPInvSeq.FromDate = ldtFromDate
           PPInvSeq.ToDate   = ldtToDate
           PPInvSeq.InvSeq   = NEXT-VALUE(PPInvSeq)
           PPInvSeq.Billed   = FALSE.
      END.

      RETURN PPInvSeq.InvSeq.
   
   END.
   
END.


