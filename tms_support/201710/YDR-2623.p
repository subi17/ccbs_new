
FOR EACH custcat WHERE custcat.category = "44" OR custcat.category = "45" NO-LOCK BY custcat.CatName :
    DISP custcat.
END. 
 
/* YDR-2623 02/10/17 */

/* Code 44 */
FIND custcat WHERE custcat.Category = "44"
               AND custcat.Segment  = "SOHO-AUTONOMO"
           EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE custcat THEN
DO:
    MESSAGE "Current value for CustCat.Category = 44 is:" SKIP
            CustCat.CatName SKIP    /* It should display "Self employee NIF" */
            "Assigning value:"
            "SOHO " + CustCat.CatName SKIP
            "Do you want to continue?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChange44 AS LOGICAL.
    IF lChange44 THEN
    DO:
        ASSIGN CustCat.CatName = "SOHO " + CustCat.CatName. 
        MESSAGE "CustCat.Category = 44 record has been updated with value:" CustCat.CatName VIEW-AS ALERT-BOX.
    END.
    ELSE
        MESSAGE "CustCat.Category = 44 record has NOT been updated" VIEW-AS ALERT-BOX.
END.
ELSE
    MESSAGE "CustCat.Category = 44 record DOES NOT EXIST!!!!! (or ambiguity)" VIEW-AS ALERT-BOX.
            
/* Code 45 */            
FIND custcat WHERE custcat.Category = "45"
               AND custcat.Segment  = "SOHO-AUTONOMO"
           EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE custcat THEN
DO:
    MESSAGE "Current value for CustCat.Category = 45 is:" SKIP
            CustCat.CatName SKIP    /* It should display "Self employee NIF" */
            "Assigning value:"
            "SOHO " + CustCat.CatName SKIP
            "Do you want to continue?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChange45 AS LOGICAL.
    IF lChange45 THEN
    DO:
        ASSIGN CustCat.CatName = "SOHO " + CustCat.CatName. 
        MESSAGE "CustCat.Category = 45 record has been updated with value:" CustCat.CatName VIEW-AS ALERT-BOX.
    END.
    ELSE
        MESSAGE "CustCat.Category = 45 record has NOT been updated" VIEW-AS ALERT-BOX.
END.
ELSE
    MESSAGE "CustCat.Category = 45 record DOES NOT EXIST!!!!! (or ambiguity)" VIEW-AS ALERT-BOX.
                               
