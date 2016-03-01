DEFINE VARIABLE llShouldBeInHPD AS LOGICAL INITIAL FALSE NO-UNDO.

FOR
   FIRST DumpFile FIELDS (Brand DumpName ConfigParam) NO-LOCK WHERE
      DumpFile.Brand = "1"                       AND
      DumpFile.DumpName = "HPD_ServiceLCounter",
   FIRST ServiceLimit FIELDS (SlSeq GroupCode) NO-LOCK WHERE
      ServiceLimit.SlSeq = ServiceLCounter.SlSeq:

   IF LOOKUP(ServiceLimit.GroupCode,DumpFile.ConfigParam) > 0
   THEN llShouldBeInHPD = TRUE.
END.

/* We won't send ServiceLCounter if it shouldn't be in HPD */
IF NOT llShouldBeInHPD
THEN RETURN.