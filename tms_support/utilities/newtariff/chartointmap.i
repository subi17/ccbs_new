DEFINE TEMP-TABLE ttCharToIntMap NO-UNDO 
   FIELD charval    AS CHARACTER
   FIELD intval     AS INTEGER
   FIELD FieldName  AS CHARACTER
   INDEX FieldName IS PRIMARY UNIQUE FieldName CharVal
   .

FUNCTION fCreatettCharToIntMap RETURNS LOGICAL
   ( icFieldName AS CHARACTER,
     icCharVal   AS CHARACTER,
     iiIntVal    AS INTEGER):
        
   CREATE ttCharToIntMap.
   ASSIGN
      ttCharToIntMap.FieldName = icFieldName
      ttCharToIntMap.charval   = icCharVal
      ttCharToIntMap.intval    = iiIntVal.
      
   RETURN FALSE.

END FUNCTION.

FUNCTION fCharToInt RETURNS INTEGER
   ( icFieldName AS CHARACTER,
     icCharVal   AS CHARACTER):
        
   FIND ttCharToIntMap WHERE
      ttCharToIntMap.FieldName = icFieldName AND
      ttCharToIntMap.charval   = icCharVal
   NO-ERROR.
   
   IF NOT AVAILABLE ttCharToIntMap
   THEN RETURN 0.
   
   RETURN ttCharToIntMap.intval.

END FUNCTION.

/* These are used in FMItems */
fCreatettCharToIntMap("FeeCalc","Full",1).
fCreatettCharToIntMap("FeeCalc","Relative",0).
fCreatettCharToIntMap("FeeCalc","UsageBased",2). 