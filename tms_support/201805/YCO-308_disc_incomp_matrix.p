/* YCO-308. Updating Discounts incompatibility matrix */

DEF VAR lcHave   AS CHAR NO-UNDO.
DEF VAR lcGet    AS CHAR NO-UNDO.
DEF VAR lcResult AS CHAR NO-UNDO.
DEF VAR lcLine   AS CHAR NO-UNDO.

DEFINE TEMP-TABLE TMP-Matrix 
   FIELD have   AS CHAR 
   FIELD get    AS CHAR 
   FIELD result AS CHAR.
   
INPUT FROM VALUE("/tmp/IncompMatrix.txt"). 
REPEAT:
   IMPORT UNFORMATTED lcLine.
   ASSIGN 
      lcHave   = TRIM(ENTRY(1, lCLine, CHR(9)))
      lcGet    = TRIM(ENTRY(2, lcLine, CHR(9)))
      lcResult = TRIM(ENTRY(3, lcLine, CHR(9))).
   IF lcResult EQ "" THEN 
      NEXT.   
   CREATE TMP-Matrix.
   ASSIGN
      TMP-Matrix.have = lcHave
      TMP-Matrix.get  = lcGet            
      TMP-Matrix.result = (IF lcResult EQ lcHave THEN "ParentValue" ELSE "ChildValue").                      
END. 
INPUT CLOSE.
OUTPUT TO VALUE("/tmp/matrix_jotorres_1.log").
FOR EACH TMSRelation NO-LOCK WHERE
         TMSRelation.TableName EQ "DiscountPlan"  AND  
         TMSRelation.KeyType   EQ "Compatibility":
   IF CAN-FIND(FIRST TMP-Matrix WHERE 
                     TMP-Matrix.have   EQ TMSRelation.ParentValue  AND 
                     TMP-Matrix.get    EQ TMSRelation.ChildValue   AND 
                     TMP-Matrix.result <> TMSRelation.RelationType) 
   THEN DO:
      PUT UNFORMATTED TMSRelation.ParentValue ";" TMSRelation.ChildValue ";" TMSRelation.RelationType SKIP.        
      Syst.TMSRelation:mEndRelation("DiscountPlan",
                                    "Compatibility",
                                    TMSRelation.ParentValue,
                                    TMSRelation.ChildValue,
                                    YES).                                         
   END.                                                                  
END. 
OUTPUT CLOSE.
OUTPUT TO VALUE("/tmp/matrix_jotorres_2.log").
FOR EACH TMP-Matrix NO-LOCK:  
   IF NOT CAN-FIND(FIRST TMSRelation WHERE 
                         TMSRelation.TableName    EQ "DiscountPlan"  AND  
                         TMSRelation.KeyType      EQ "Compatibility" AND    
                         TMSRelation.ParentValue  EQ TMP-Matrix.have AND 
                         TMSRelation.ChildValue   EQ TMP-Matrix.get  AND 
                         TMSRelation.RelationType EQ TMP-Matrix.result)
    THEN DO:
       Syst.TMSRelation:mAddRelation("DiscountPlan",
                                     "Compatibility",
                                     TMP-Matrix.have,
                                     TMP-Matrix.get ,
                                     TMP-Matrix.result,
                                     YES).
      PUT UNFORMATTED  TMP-Matrix.have ";" TMP-Matrix.get ";" TMP-Matrix.result SKIP.  
    END.     
      
END.
OUTPUT CLOSE. 
