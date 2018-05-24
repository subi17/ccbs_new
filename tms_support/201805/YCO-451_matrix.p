/*----------------------------------------------
YCO-451. 
tms_support/201805/YCO-451_matrix.p
Updating discounts incompatibility matrix 
jotorres 22.5.2018
----------------------------------------------*/

DEF VAR lcHave   AS CHAR NO-UNDO.
DEF VAR lcGet    AS CHAR NO-UNDO.
DEF VAR lcResult AS CHAR NO-UNDO.
DEF VAR lcLine   AS CHAR NO-UNDO.

DEFINE TEMP-TABLE TMP-Matrix 
   FIELD have   AS CHAR 
   FIELD get    AS CHAR 
   FIELD result AS CHAR.
   
INPUT FROM VALUE("/tmp/YCO-451_Incomp_Matrix.txt"). 
REPEAT:
   IMPORT UNFORMATTED lcLine.
   IF NUM-ENTRIES(lCLine, CHR(9)) <> 3 THEN
      NEXT.
   ASSIGN 
      lcHave   = TRIM(ENTRY(1, lCLine, CHR(9)))
      lcGet    = TRIM(ENTRY(2, lcLine, CHR(9)))
      lcResult = TRIM(ENTRY(3, lcLine, CHR(9))).
   IF lcResult EQ "" THEN 
      NEXT.
   IF lcHave = lcGet THEN
      NEXT.   
   CREATE TMP-Matrix.
   ASSIGN
      TMP-Matrix.have = lcHave
      TMP-Matrix.get  = lcGet            
      TMP-Matrix.result = (IF lcResult EQ lcHave THEN "ParentValue" ELSE "ChildValue").                      
END. 
INPUT CLOSE.
OUTPUT TO VALUE("/tmp/YCO-451_Incomp_Matrix_1.log").
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
OUTPUT TO VALUE("/tmp/YCO-451_Incomp_Matrix_2.log").
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
