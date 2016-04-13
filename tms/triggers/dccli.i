FUNCTION fCheckFixedFee RETURNS LOGICAL
   (icMsSeq         AS CHARACTER,
    icPerContractID AS CHARACTER):
   
   DEFINE VARIABLE llFound AS LOGICAL INITIAL FALSE NO-UNDO.
      
   FOR
      EACH FixedFee FIELDS (Brand HostTable KeyValue SourceTable SourceKey) NO-LOCK WHERE
         FixedFee.Brand       = "1"      AND
         FixedFee.HostTable   = "MobSub" AND
         FixedFee.KeyValue    = icMsSeq  AND
         FixedFee.SourceTable = "DCCLI":
            
      llFound = TRUE.
      
      IF FixedFee.SourceKey NE icPerContractID
      THEN RETURN FALSE.
   END.

   RETURN llFound.

END FUNCTION.