USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.CustomerDump FINAL INHERITS GenericDump:
  
   CONSTRUCTOR PUBLIC CustomerDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC CustomerDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
      
      SUPER(iiDumpID, icFile).
      
   END CONSTRUCTOR.    


   METHOD PRIVATE LOGICAL mSelfEmployed():

      FOR
         FIRST CustCat FIELDS (Brand Category SelfEmployed) NO-LOCK WHERE
            CustCat.Brand    = "1" AND
            CustCat.Category = lhMainTable::Category
         RETURN CustCat.SelfEmployed.
      END.

      RETURN FALSE.

   END METHOD.


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "Employer"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lhMainTable::CustIDType NE "CIF" AND lhMainTable::Profession > ""
                          THEN lhMainTable::CompanyName
                          ELSE "".
         WHEN "SelfEmployed"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lhMainTable::CustIDType = "passport"
                          THEN "no"
                          ELSE STRING(mSelfEmployed()).                          
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

END CLASS.