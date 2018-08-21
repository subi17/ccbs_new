USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.OrderCustomerDump FINAL INHERITS GenericDump:
  
   CONSTRUCTOR PUBLIC OrderCustomerDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC OrderCustomerDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
      
      SUPER(iiDumpID, icFile).
      
   END CONSTRUCTOR.
   

   METHOD PRIVATE CHARACTER mGetSegment():

      FOR
         FIRST CustCat FIELDS (Brand Category Segment) NO-LOCK WHERE
            CustCat.Brand    = "1" AND
            CustCat.Category = lhMainTable::Category:
         RETURN CustCat.Segment.
      END.

      RETURN "".

   END METHOD.

   METHOD PRIVATE CHARACTER mLanguage():

      DEFINE VARIABLE liLanguage AS INTEGER NO-UNDO.
      ASSIGN
         liLanguage = INTEGER(lhMainTable::Language) NO-ERROR.

      IF ERROR-STATUS:ERROR
      THEN RETURN "es".

      FOR FIRST Language NO-LOCK WHERE
          Language.Language = liLanguage:
         RETURN Language.LanguageCode.
      END.

      RETURN "es".

   END METHOD.

   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "KeyValue"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN {HPD/keyvalue.i lhMainTable :: objDumpBase:lcKeyDelimiter OrderId RowType}.
         WHEN "Segment"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetSegment().
         WHEN "Language"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mLanguage().
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

END CLASS.