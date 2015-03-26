FUNCTION checkChar RETURN LOGICAL (
   INPUT pcDbTableField AS CHARACTER,
   INPUT pcVal1 AS CHARACTER,
   INPUT pcval2 AS CHARACTER):

   IF pcVal1 ne pcVal2 THEN
      MESSAGE "Error comparing character database field " 
         pcDbTableField + 
         ", database value: " + pcVal1 + 
         ", variable value: " + pcVal2         
         VIEW-AS ALERT-BOX.
   assert_equal_char(pcVal1, pcVal2).
END.

FUNCTION checkLogical RETURN LOGICAL (
   INPUT pcDbTableField AS CHARACTER,
   INPUT plVal1 AS LOGICAL,
   INPUT plval2 AS LOGICAL,
   INPUT pcErrorMsg AS CHARACTER):

   IF plVal1 ne plVal2 THEN
   DO:
      MESSAGE "Error comparing logical database field: " 
         pcDbTableField + 
         ", database value: " + STRING(plVal1) + 
         ", variable value: " + STRING(plVal2) 
        VIEW-AS ALERT-BOX.
   END.
   assert(plVal1 eq plVal2, pcErrorMsg).
END.


FUNCTION checkDecimal RETURN LOGICAL ( 
   INPUT pcDbTableField AS CHARACTER,
   INPUT pdeVal1 AS DECIMAL,
   INPUT pdeVal2 AS DECIMAL):

   IF pdeVal1 ne pdeVal2 THEN
      MESSAGE "Error comparing decimal database field: " 
         pcDbTableField + 
         ", database value: " + STRING(pdeVal1) + 
         ", variable value: " + STRING(pdeVal2) 
         VIEW-AS ALERT-BOX.
   assert_equal_double(pdeVal1, pdeVal2).
END.


FUNCTION checkDecimalWithDiff RETURN LOGICAL ( 
   INPUT pcDbTableField AS CHARACTER,
   INPUT pdeVal1 AS DECIMAL,
   INPUT pdeVal2 AS DECIMAL,
   INPUT pdeMaxDiff AS DECIMAL ):

   DEFINE VARIABLE deDiff AS DECIMAL NO-UNDO. 
   IF pdeVal2 > pdeVal1 THEN 
      deDiff = pdeVal2 - pdeVal1.
   ELSE
      deDiff = pdeVal1 - pdeVal2.

   IF deDiff > pdeMaxDiff THEN
   DO:
      MESSAGE "Error comparing decimal database field: " 
         pcDbTableField + 
         ", database value: " + STRING(pdeVal1) + 
         ", variable value: " + STRING(pdeVal2) +
         ", difference bigger than " pdeMaxDiff
         VIEW-AS ALERT-BOX.
      assert(TRUE, "Decimal database field " + pcDbTableField + " is incorrect.").
   END.
END.  


FUNCTION checkDate RETURN LOGICAL (
   INPUT pcDbTableField AS CHARACTER,
   INPUT pdaVal1 AS DATE,
   INPUT pdaVal2 AS DATE):
  
   IF pdaVal1 ne pdaVal2 THEN
      MESSAGE "Error comparing date database field: " 
         pcDbTableField +
         ", database value: " + STRING(pdaVal1) + 
         ", variable value: " + STRING(pdaVal2) 
         VIEW-AS ALERT-BOX.
   assert_equal_date(pdaVal1, pdaVal2).
END.

FUNCTION checkDateTime RETURN LOGICAL 
   (INPUT pcDbTableField AS CHARACTER,
   INPUT pdaVal1 AS DATETIME,
   INPUT pdaVal2 AS DATETIME):
  
   IF pdaVal1 ne pdaVal2 THEN
      MESSAGE "Error comparing datetime database field: " 
         pcDbTableField +
         ", database value: " + STRING(pdaVal1) + 
         ", variable value: " + STRING(pdaVal2) 
         VIEW-AS ALERT-BOX.
   assert(pdaVal1 eq pdaVal2, "").
END.


FUNCTION checkInt RETURN LOGICAL (
   INPUT pcDbTableField AS CHARACTER,
   INPUT piVal1 AS INTEGER,
   INPUT piVal2 AS INTEGER):
  
   IF piVal1 ne piVal2 THEN
      MESSAGE "Error comparing integer database field: " 
         pcDbTableField +
         ", database value: " + STRING(piVal1) + 
         ", variable value: " + STRING(piVal2) 
         VIEW-AS ALERT-BOX.
   assert_equal_int(piVal1, piVal2).
END.

