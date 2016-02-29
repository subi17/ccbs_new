USING Progress.Lang.*.
USING HPD.GenericDump.
USING HPD.HPDCommon.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.InvRowDump FINAL INHERITS GenericDump:


   CONSTRUCTOR PUBLIC InvRowDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC InvRowDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
      
      SUPER(iiDumpID, icFile).
      
   END CONSTRUCTOR.    


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "InvRowKey"
         THEN IF ilCheckOnly
              THEN RETURN "OK".            
              ELSE RETURN {HPD/keyvalue.i lhMainTable :: objDumpBase:lcKeyDelimiter InvNum SubInvNum InvRowNum}. 
         WHEN "Amt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(ROUND(lhMainTable::Amt,2)).
         WHEN "VATAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(HPDCommon:mVATAmt(lhMainTable::Amt, lhMainTable::VATPerc)).
         WHEN "TotalAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(ROUND(lhMainTable::Amt + HPDCommon:mVATAmt(lhMainTable::Amt, lhMainTable::VATPerc),2)).
         OTHERWISE RETURN "".          
         
      END CASE.
   
   END METHOD.

END CLASS.