/*
   Include file parameters:
      1 = table name
      2 = table name capital letters
      3 = database name
      4...12 = keyvalue fields (at least one is required)
*/

TRIGGER PROCEDURE FOR REPLICATION-DELETE OF {1}.

{HPD/HPDConst.i}

&IF {&{2}_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW {1}
THEN RETURN.

CREATE {3}.RepLog.
ASSIGN
   {3}.RepLog.TableName = "{1}"
   {3}.RepLog.EventType = "DELETE"
   {3}.RepLog.EventTime = NOW
   {3}.RepLog.KeyValue  = &IF '{4}' NE ''
                          &THEN
                          STRING({1}.{4})
                          &ENDIF
                          &IF '{5}' NE ''
                          &THEN
                          + {&HPDKeyDelimiter} + STRING({1}.{5})
                          &ENDIF
                          &IF '{6}' NE ''
                          &THEN
                          + {&HPDKeyDelimiter} + STRING({1}.{6})
                          &ENDIF
                          &IF '{7}' NE ''
                          &THEN
                          + {&HPDKeyDelimiter} + STRING({1}.{7})
                          &ENDIF
                          &IF '{8}' NE ''
                          &THEN
                          + {&HPDKeyDelimiter} + STRING({1}.{8})
                          &ENDIF
                          &IF '{9}' NE ''
                          &THEN
                          + {&HPDKeyDelimiter} + STRING({1}.{9})
                          &ENDIF
                          &IF '{10}' NE ''
                          &THEN
                          + {&HPDKeyDelimiter} + STRING({1}.{10})
                          &ENDIF
                          &IF '{11}' NE ''
                          &THEN
                          + {&HPDKeyDelimiter} + STRING({1}.{11})
                          &ENDIF
                          &IF '{12}' NE ''
                          &THEN
                          + {&HPDKeyDelimiter} + STRING({1}.{12})
                          &ENDIF
   .

&ENDIF