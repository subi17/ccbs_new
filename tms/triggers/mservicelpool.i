DEFINE BUFFER lbMServiceLPool FOR MServiceLPool.

/* We will send only the newest one */
FOR
   FIRST lbMServiceLPool FIELDS (MsSeq SLSeq EndTS) NO-LOCK USE-INDEX MsSeq WHERE
      lbMServiceLPool.MsSeq = MServiceLPool.MsSeq  AND
      lbMServiceLPool.SlSeq = MServiceLPool.SlSeq:

   IF ROWID(lbMServiceLPool) NE ROWID(MServiceLPool) AND
      lbMServiceLPool.EndTS > MServiceLPool.EndTS
   THEN RETURN.
END.
