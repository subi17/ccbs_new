USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.RepTextDump FINAL INHERITS GenericDump:
  
   CONSTRUCTOR PUBLIC RepTextDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC RepTextDump
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
              ELSE RETURN {HPD/keyvalue.i lhMainTable :: objDumpBase:lcKeyDelimiter TextType LinkCode Language ToDate}.
         WHEN "RepText"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lhMainTable::RepText = "" THEN "" ELSE mRemoveLFCR(lhMainTable::RepText).
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD PRIVATE CHARACTER mRemoveLFCR
      (icText AS CHARACTER):
         
      RETURN REPLACE(REPLACE(icText,CHR(10)," "),CHR(13)," ").
      
   END METHOD.

END CLASS.