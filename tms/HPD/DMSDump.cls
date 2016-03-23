USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DMSDump FINAL INHERITS GenericDump:
  
   CONSTRUCTOR PUBLIC DMSDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC DMSDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
      
      SUPER(iiDumpID, icFile).
      
   END CONSTRUCTOR.    


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "DMSStatusTS"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lhMainTable::DMSStatusTS = 0 THEN "" ELSE TRIM(STRING(lhMainTable::DMSStatusTS,"->>>>>>>>>>>>>>>>>>>>>>9.9<<<<<<<<<")).
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

END CLASS.