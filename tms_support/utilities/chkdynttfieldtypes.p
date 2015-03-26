DEFINE VARIABLE tttable AS HANDLE NO-UNDO. 
DEFINE VARIABLE tttablebuf AS HANDLE NO-UNDO. 

DEFINE TEMP-TABLE ttdatatypes
  FIELD cdatatype AS CHARACTER
  INDEX idxdatatype cdatatype.



FUNCTION fAddDataType RETURN LOGICAL (INPUT pcdatatype AS CHARACTER):
   IF NOT CAN-FIND(ttdatatypes WHERE cdatatype = pcdatatype) THEN
   DO:
      CREATE ttdatatypes.
      ASSIGN cdatatype = pcdatatype.
   END.

END.



FUNCTION fAddDataTypes RETURN LOGICAL (INPUT pcTableName AS CHARACTER):
  CREATE TEMP-TABLE tttable.

  tttable:CREATE-LIKE(pcTableName).
   tttable:TEMP-TABLE-PREPARE("tt" + pcTableName).
   tttablebuf = tttable:DEFAULT-BUFFER-HANDLE.
   
   DEFINE VARIABLE iField AS INTEGER NO-UNDO. 
   REPEAT iField = 1 TO tttableBuf:NUM-FIELDS:
      DEFINE VARIABLE hField AS HANDLE NO-UNDO. 
      hField = tttableBuf:BUFFER-FIELD(iField).
      fAddDataType(hField:DATA-TYPE).
   END.

  DELETE OBJECT tttable.
END.

fAddDataTypes("Order").
fAddDataTypes("Invoice").
fAddDataTypes("SIM").
fAddDataTypes("PrepaidRequest").
fAddDataTypes("OrderCustomer").
fAddDataTypes("OrderAccessory").
fAddDataTypes("OrderTopup").
fAddDataTypes("DCCLI").
fAddDataTypes("BillItem").
fAddDataTypes("Customer").
fAddDataTypes("InvRow").
fAddDataTypes("MnpProcess").
fAddDataTypes("MnpMessage").




DEFINE STREAM sDataTypes.
OUTPUT STREAM sDataTypes to value("/home/harrim/exportdata/datatypelist.txt").
FOR EACH ttdatatypes:
   PUT STREAM sDataTypes UNFORMATTED cdatatype SKIP.

END.

OUTPUT STREAM sDataTypes CLOSE.
