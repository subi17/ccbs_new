FUNCTION fCheckHPDStatus RETURNS LOGICAL
   (iiMSSeq AS INTEGER,
    iiCustNum AS INTEGER):

   IF iiCustNum > 0
   THEN RETURN CAN-FIND(FIRST Customer NO-LOCK WHERE Customer.CustNum = iiCustNum).

   RETURN CAN-FIND(FIRST MobSub NO-LOCK WHERE MobSub.MsSeq = iiMsSeq).
END.

/* We will send only the newest one */
DEFINE BUFFER lbMServiceLimit FOR MServiceLimit.

FOR
   FIRST lbMServiceLimit FIELDS (MsSeq DialType SLSeq EndTS) NO-LOCK USE-INDEX msseq WHERE
      lbMServiceLimit.MsSeq    = MServiceLimit.MsSeq    AND
      lbMServiceLimit.DialType = MServiceLimit.DialType AND
      lbMServiceLimit.SlSeq    = MServiceLimit.SlSeq:

   IF ROWID(lbMServiceLimit) NE ROWID(MServiceLimit) AND
      lbMServiceLimit.EndTS > MServiceLimit.EndTS
   THEN RETURN.
END.
