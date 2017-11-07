&IF "{&COMMISSION_I}" NE "YES"
&THEN
&GLOBAL-DEFINE COMMISSION_I YES

{Syst/commali.i}

DEF BUFFER bTargStatus FOR CoTarg.

FUNCTION fCommStatus RETURNS LOGIC
   (iiCoTargID AS INT,
    iiStatus   AS INT,
    iiReason   AS INT,
    icMessage  AS CHAR):

   DO TRANS:
      FIND FIRST bTargStatus WHERE bTargStatus.CoTargID = iiCoTargID
         EXCLUSIVE-LOCK.
         
      ASSIGN 
         bTargStatus.HandledTS    = Func.Common:mMakeTS()
         bTargStatus.CommStatus   = iiStatus
         bTargStatus.StatusReason = iiReason.
         
      IF icMessage > "" THEN DO:
         Func.Common:mWriteMemo("CoTarg",
                          STRING(bTargStatus.CoTargID),
                          0,
                          "COMMISSION",
                          icMessage).
      END.
                           
      RELEASE bTargStatus.
   END.
    
END FUNCTION.

&ENDIF
