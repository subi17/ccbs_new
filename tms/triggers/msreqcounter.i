FUNCTION fCreateMsReqCounter RETURNS LOGICAL
   (iiReqType   AS INTEGER,
    iiReqStatus AS INTEGER,
    iiAmount    AS INTEGER):

   DEFINE VARIABLE liCurrentNumber AS INTEGER NO-UNDO.

   DO TRANSACTION:
   
      FOR
         EACH MsReqCounter NO-LOCK WHERE
            MsReqCounter.ReqType   = iiReqType    AND 
            MsReqCounter.ReqStatus = iiReqStatus:
   
         liCurrentNumber = MsReqCounter.TransNbr.
         
         BUFFER MsReqCounter:FIND-CURRENT(EXCLUSIVE-LOCK, NO-WAIT).
         IF NOT AVAILABLE MsReqCounter
         THEN NEXT.
         
         MsReqCounter.ReqStatusCount = MsReqCounter.ReqStatusCount + iiAmount.

         RELEASE MsReqCounter.         
         RETURN TRUE.
         
      END.
      
      CREATE MsReqCounter.
      ASSIGN
         MsReqCounter.ReqType        = iiReqType
         MsReqCounter.ReqStatus      = iiReqStatus
         MsReqCounter.TransNbr       = liCurrentNumber + 1
         MsReqCounter.ReqStatusCount = iiAmount
         .
      
      RELEASE MsReqCounter.

   END.

   RETURN TRUE.

END FUNCTION.  
