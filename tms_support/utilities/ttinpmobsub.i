DEFINE TEMP-TABLE ttinpmobsub
   FIELD coption AS CHARACTER
   FIELD cvalue  AS CHARACTER
   INDEX idxoption coption.

FUNCTION fAddOption RETURN LOGICAL 
   (INPUT pcOption AS CHARACTER, INPUT pcValue AS CHARACTER):
   IF NOT CAN-FIND(ttinpmobsub WHERE coption = pcOption) THEN
   DO:
      CREATE ttinpmobsub.
      ttinpmobsub.coption = pcOption.
      ttinpmobsub.cvalue = pcValue.
      RETURN TRUE.
   END.
   RETURN FALSE.
END.
