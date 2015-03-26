FUNCTION fStreamSequence RETURNS INTEGER
  (INPUT pdaDate AS DATE, INPUT piStream AS INTEGER):
   
   FIND FIRST DtlSeq WHERE
              DtlSeq.SeqDate   = pdaDate AND
              DtlSeq.SeqStream = piStream
   EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL DtlSeq THEN DO:
      CREATE DtlSeq.
      ASSIGN
         DtlSeq.SeqDate   = pdaDate
         DtlSeq.SeqStream = piStream
         DtlSeq.SeqVal    = piStream * 10000000.
   END.

   DtlSeq.SeqVal = DtlSeq.SeqVal + 1.

   RETURN DtlSeq.SeqVal.
END.

FUNCTION fFindMcdrdtl RETURN LOG
 (INPUT idaDate  AS DATE, 
  INPUT iiDTLseq AS INTEGER).
 
   FIND FIRST mcdrdtl.McdrDtl WHERE 
              mcdrdtl.McdrDtl.datest = idaDate AND 
              mcdrdtl.McdrDtl.dtlseq = iiDTLSeq
   EXCLUSIVE-LOCK NO-ERROR.

END.

FUNCTION fFindMcdrdtl2 RETURN LOG
 (INPUT idaDate  AS DATE,
  INPUT iiDTLseq AS INTEGER).

   FIND FIRST mcdrdtl.McdrDtl2 WHERE
              mcdrdtl.McdrDtl2.datest = idaDate AND
              mcdrdtl.McdrDtl2.dtlseq = iiDTLSeq
   EXCLUSIVE-LOCK NO-ERROR.

END.
