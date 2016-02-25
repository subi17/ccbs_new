USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DayCampaignDump FINAL INHERITS GenericDump:
  
   CONSTRUCTOR PUBLIC DayCampaignDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC DayCampaignDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
      
      SUPER(iiDumpID, icFile).
      
   END CONSTRUCTOR.    


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "DSS2Compatible"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(lhMainTable::DCevent = "CONT15" OR
                                 CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                    CLIType.Brand    = "1"                  AND
                                    CLIType.CLIType  = lhMainTable::DCevent AND
                                    CLIType.LineType > 0),
                                 "yes/no").
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

END CLASS.