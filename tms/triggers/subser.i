DEFINE BUFFER lbSubSer FOR SubSer.

/* We will send only the newest one */
FOR
   FIRST lbSubSer FIELDS (MsSeq ServCom SSDate) NO-LOCK USE-INDEX ServCom WHERE
      lbSubSer.MsSeq   = SubSer.MsSeq  AND
      lbSubSer.ServCom = SubSer.ServCom:

   IF ROWID(lbSubSer) NE ROWID(SubSer) AND
      lbSubSer.SSDate > SubSer.SSDate
   THEN RETURN.
END.