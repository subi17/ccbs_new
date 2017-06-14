USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.CustomerDump FINAL INHERITS GenericDump:

/*   DEFINE PRIVATE VARIABLE lcSelfEmpCategories AS CHARACTER NO-UNDO. */
  
   CONSTRUCTOR PUBLIC CustomerDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC CustomerDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

/*
      /* When doing full dump this will speed up
         things a tiny bit */
      mGetSelfEmpCategories().
*/
      SUPER(iiDumpID, icFile).

   END CONSTRUCTOR.    

/*
   METHOD PRIVATE VOID mGetSelfEmpCategories():

      FOR
         EACH CustCat FIELDS (Brand Category SelfEmployed) NO-LOCK WHERE
            CustCat.Brand        = "1" AND
            CustCat.SelfEmployed = TRUE:

         lcSelfEmpCategories = lcSelfEmpCategories +
                               (IF lcSelfEmpCategories = "" THEN "" ELSE ",") +
                               CustCat.Category.
      END.

   END METHOD.
*/

   METHOD PRIVATE LOGICAL mSelfEmployed():

      FOR
         FIRST CustCat FIELDS (Brand Category SelfEmployed) NO-LOCK WHERE
            CustCat.Brand    = "1" AND
            CustCat.Category = lhMainTable::Category:
         RETURN CustCat.SelfEmployed.
      END.

      RETURN FALSE.

   END METHOD.

   METHOD PRIVATE CHARACTER mGetSegment():

      FOR
         FIRST CustCat FIELDS (Brand Category Segment) NO-LOCK WHERE
            CustCat.Brand    = "1" AND
            CustCat.Category = lhMainTable::Category:
         RETURN CustCat.Segment.
      END.

      RETURN "".

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
                          /*
                          ELSE IF lcSelfEmpCategories > ""
                          THEN STRING(LOOKUP(lhMainTable::Category, lcSelfEmpCategories) > 0)
                          */
                          ELSE STRING(mSelfEmployed()).
         WHEN "Segment"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetSegment().
         OTHERWISE RETURN "".
      END CASE.

   END METHOD.

END CLASS.