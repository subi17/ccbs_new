USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.OrderProductParamDump FINAL INHERITS GenericDump:
  
   CONSTRUCTOR PUBLIC OrderProductParamDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC OrderProductParamDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
      
      SUPER(iiDumpID, icFile).
      
   END CONSTRUCTOR.    


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "KeyValue"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN {HPD/keyvalue.i lhMainTable :: objDumpBase:lcKeyDelimiter OrderProductID ParamName}.
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

END CLASS.