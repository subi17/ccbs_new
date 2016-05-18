USING Progress.Lang.*.
USING HPD.GenericDump.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.OrderActionDump FINAL INHERITS GenericDump:
  
   CONSTRUCTOR PUBLIC OrderActionDump
      (iiDumpID AS INTEGER):
               
      SUPER(iiDumpID).
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC OrderActionDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
      
      SUPER(iiDumpID, icFile).
      
   END CONSTRUCTOR.    


   METHOD PRIVATE CHARACTER mItemTypeDesc():
   
      IF lhMainTable::ItemType NE "BundleItem"
      THEN RETURN lhMainTable::ItemType.
      
      IF INDEX(lhMainTable::ItemKey,"MDUB") > 0 OR
         INDEX(lhMainTable::ItemKey,"DATA") > 0
      THEN RETURN "DataBundle".
      
      FOR
         Order FIELDS (Brand OrderID CLIType) NO-LOCK WHERE
            Order.Brand   = "1" AND
            Order.OrderId = lhMainTable::OrderId:
   
         IF INDEX(lhMainTable::ItemKey,Order.CLIType) > 0
         THEN RETURN "TariffBundle".
      END.
   
      RETURN "BundleItem".

   END METHOD.


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "KeyValue"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN {HPD/keyvalue.i lhMainTable :: objDumpBase:lcKeyDelimiter OrderId ItemType ItemKey}.
         WHEN "ItemTypeDesc"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mItemTypeDesc().
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.

END CLASS.