DEFINE VARIABLE lcBundles       AS CHARACTER             NO-UNDO.
DEFINE VARIABLE llShouldBeInHPD AS LOGICAL INITIAL FALSE NO-UNDO.

FOR FIRST DumpFile FIELDS (Brand DumpName ConfigParam) NO-LOCK WHERE
   DumpFile.Brand = "1" AND
   DumpFile.DumpName = "HPD_ServiceLCounter":

   lcBundles = DumpFile.ConfigParam.      
      
END.

BUNDLELOOP:
DO lii = 1 TO NUM-ENTRIES(lcBundles):
   FOR
      FIRST ServiceLimit FIELDS (SlSeq GroupCode) NO-LOCK WHERE
         ServiceLimit.SlSeq     = ServiceLCounter.SlSeq AND
         ServiceLimit.GroupCode = ENTRY(lii, lcBundles):
      llShouldBeInHPD = TRUE.
      LEAVE BUNDLELOOP.
   END.
END.

/* We won't send ServiceLCounter if it shouldn't be in HPD */
IF NOT llShouldBeInHPD
THEN RETURN.