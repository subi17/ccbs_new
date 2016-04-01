FUNCTION fCheckHPDStatus RETURNS LOGICAL
   (iiMSSeq AS INTEGER,
    iiSLSeq AS INTEGER,
    iiCustNum AS INTEGER):
       
   FOR FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
      MobSub.MsSeq = iiMsSeq:
   
      IF iiCustNum > 0 AND
         CAN-FIND(FIRST ServiceLimit NO-LOCK WHERE
            ServiceLimit.SlSeq = iiSlSeq AND
            ServiceLimit.GroupCode BEGINS "DSS")
      THEN RETURN TRUE.
      ELSE IF iiCustNum = 0 OR iiCustNum = ?
      THEN RETURN TRUE.
   END.
   
   RETURN FALSE.
END.

/* We will send only the newest one */
DEFINE BUFFER lbMServiceLimit FOR MServiceLimit.

FOR
   FIRST lbMServiceLimit FIELDS (MsSeq DialType SLSeq EndTS) NO-LOCK USE-INDEX msseq WHERE
      lbMServiceLimit.MsSeq    = MServiceLimit.MsSeq    AND
      lbMServiceLimit.DialType = MServiceLimit.DialType AND
      lbMServiceLimit.SlSeq    = MServiceLimit.SlSeq:

   IF lbMServiceLimit.EndTS > MServiceLimit.EndTS
   THEN RETURN.
END.
