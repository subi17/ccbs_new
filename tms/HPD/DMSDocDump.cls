USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DMSDocDump FINAL INHERITS GenericDump:
  
   CONSTRUCTOR PUBLIC DMSDocDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC DMSDocDump
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
              ELSE RETURN {HPD/keyvalue.i lhMainTable :: objDumpBase:lcKeyDelimiter DMSID DocTypeID DocStatusTS}.
         WHEN "DMSStatusTS"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lhMainTable::DMSStatusTS = 0 THEN "" ELSE TRIM(STRING(lhMainTable::DMSStatusTS,"->>>>>>>>>>>>>>>>>>>>>>9.9<<<<<<<<<")).
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

END CLASS.