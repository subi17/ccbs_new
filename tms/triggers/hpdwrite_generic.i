/*
   Include file parameters:
      1 = table name
      2 = table name capital letters
      3 = database name
      4...12 = keyvalue fields (at least one is required)
*/

TRIGGER PROCEDURE FOR REPLICATION-WRITE OF {1} OLD BUFFER old{1}.

{HPD/HPDConst.i}

&IF {&{2}_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/replog_tenantname.i}

CREATE {3}.RepLog.
ASSIGN
   {3}.RepLog.RowID      = STRING(ROWID({1}))
   {3}.RepLog.TableName  = "{1}"
   {3}.RepLog.EventType  = (IF NEW({1})
                            THEN "CREATE"
                            ELSE "MODIFY")
   {3}.RepLog.EventTime  = NOW
   {3}.RepLog.TenantName = fRepLogTenantName(BUFFER {1}:HANDLE)
   .

IF NOT NEW({1})
THEN DO: 
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE {1} USING
      {4} {5} {6} {7} {8} {9} {10} {11} {12}
   TO old{1} SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
   CREATE {3}.RepLog.
      ASSIGN
         {3}.RepLog.TableName  = "{1}"
         {3}.RepLog.EventType  = "DELETE"
         {3}.RepLog.EventTime  = NOW
         {3}.RepLog.TenantName = fRepLogTenantName(BUFFER old{1}:HANDLE)
         {3}.RepLog.KeyValue  = &IF '{4}' NE ''
                                &THEN
                                SUBSTITUTE("&1",old{1}.{4})
                                &ENDIF
                                &IF '{5}' NE ''
                                &THEN
                                + {&HPDKeyDelimiter} + SUBSTITUTE("&1",old{1}.{5})
                                &ENDIF
                                &IF '{6}' NE ''
                                &THEN
                                + {&HPDKeyDelimiter} + SUBSTITUTE("&1",old{1}.{6})
                                &ENDIF
                                &IF '{7}' NE ''
                                &THEN
                                + {&HPDKeyDelimiter} + SUBSTITUTE("&1",old{1}.{7})
                                &ENDIF
                                &IF '{8}' NE ''
                                &THEN
                                + {&HPDKeyDelimiter} + SUBSTITUTE("&1",old{1}.{8})
                                &ENDIF
                                &IF '{9}' NE ''
                                &THEN
                                + {&HPDKeyDelimiter} + SUBSTITUTE("&1",old{1}.{9})
                                &ENDIF
                                &IF '{10}' NE ''
                                &THEN
                                + {&HPDKeyDelimiter} + SUBSTITUTE("&1",old{1}.{10})
                                &ENDIF
                                &IF '{11}' NE ''
                                &THEN
                                + {&HPDKeyDelimiter} + SUBSTITUTE("&1",old{1}.{11})
                                &ENDIF
                                &IF '{12}' NE ''
                                &THEN
                                + {&HPDKeyDelimiter} + SUBSTITUTE("&1",old{1}.{12})
                                &ENDIF
         .
   END.
END.

&ENDIF