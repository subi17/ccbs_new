USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.TermReturnDump FINAL INHERITS GenericDump:
  
   CONSTRUCTOR PUBLIC TermReturnDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC TermReturnDump
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
              ELSE RETURN {HPD/keyvalue.i lhMainTable :: objDumpBase:lcKeyDelimiter OrderId ReturnTS}.
         WHEN "DeviceStart"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lhMainTable::DeviceStart = ? AND lhMainTable::DeviceScreen = ?
                          THEN "yes"
                          ELSE STRING(lhMainTable::DeviceStart,"yes/no").
         WHEN "DeviceScreen"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lhMainTable::DeviceStart = ? AND lhMainTable::DeviceScreen = ?
                          THEN "yes"
                          ELSE STRING(lhMainTable::DeviceScreen,"yes/no").
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

END CLASS.