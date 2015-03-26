DEF BUFFER xxUsage FOR HighUsage.

FOR EACH highusage  NO-LOCK.

   FIND FIRST INVseq where 
              InvSeq.InvSeq = Highusage.InvSeq no-LOCK NO-ERROR.

   IF NOT AVAIL InvSeq THEN DO:
      FIND FIRST xxusage WHERE 
           RECID(xxUsage) = RECID(HighUsage) NO-ERROR.
      DELETE xxusage .
      NEXT.
   END.
      
   IF InvSeq.Todate < today THEN DO:

      FIND FIRST xxusage WHERE
           RECID(xxUsage) = RECID(HighUsage) NO-ERROR.

      DELETE xxusage .
   END.
ENd.
