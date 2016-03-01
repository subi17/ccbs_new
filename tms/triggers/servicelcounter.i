DEFINE VARIABLE lcBundles       AS CHARACTER             NO-UNDO.
DEFINE VARIABLE llShouldBeInHPD AS LOGICAL INITIAL FALSE NO-UNDO.

FOR FIRST DumpFile FIELDS (Brand DumpName ConfigParam) NO-LOCK WHERE
   DumpFile.Brand = "1" AND
   DumpFile.DumpName = "HPD_ServiceLCounter":

   lcBundles = DumpFile.ConfigParam.      
      
END.

DEFINE VARIABLE ldeFromStamp AS DECIMAL NO-UNDO.
DEFINE VARIABLE ldeToStamp   AS DECIMAL NO-UNDO.

ASSIGN
   ldeFromStamp = ServiceLCounter.Period * 100 + 1.
   ldeToStamp   = (ServiceLCounter.Period + 1) * 100.
   .
   
BUNDLELOOP:
DO lii = 1 TO NUM-ENTRIES(lcBundles):
   FOR
      FIRST ServiceLimit FIELDS (SlSeq GroupCode DialType) NO-LOCK WHERE
         ServiceLimit.GroupCode = ENTRY(lii, lcBundles) AND
         ServiceLimit.DialType  = 7,
      FIRST MServiceLimit FIELDS (MsSeq SlSeq DialType EndTS FromTs) NO-LOCK WHERE
         MServiceLimit.MsSeq    = ServiceLCounter.MsSeq AND
         MServiceLimit.DialType = 7                     AND
         MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
         MServiceLimit.EndTS   >= ldeFromStamp          AND
         MServiceLimit.FromTS   < ldeToStamp:

      llShouldBeInHPD = TRUE.
      LEAVE BUNDLELOOP.
   END.
END.

/* We won't send ServiceLCounter if it shouldn't be in HPD */
IF NOT llShouldBeInHPD
THEN RETURN.