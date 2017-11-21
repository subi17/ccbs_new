DEFINE VARIABLE lcOldCategory AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFile        AS CHARACTER NO-UNDO.
DEFINE VARIABLE llSimul       AS LOGICAL   NO-UNDO INITIAL TRUE.

MESSAGE "YDR-2621 - Change TMS Category for some customers" SKIP(1)
        "Customers with CIF beginning V00, and category 20 or blank category," SKIP
        "will get category 30" SKIP(1)
        "Do you want to execute in Simulation mode?" SKIP(1)
        "This mode will generate a file with customers to change" SKIP
        "but no data change will be made"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE llSimul.

IF llSimul THEN
    lcFile = "/tmp/YDR-2621_Simulation_" + STRING(TIME, "HH:MM:SS") + ".log".
ELSE 
    lcFile = "/tmp/YDR-2621_" + STRING(TIME, "HH:MM:SS") + ".log".

OUTPUT TO VALUE(lcFile).
   PUT UNFORMATTED "Custnum;CustIdType;OrgId;Old Category;Error (if any)" SKIP.                                             
   
   FOR EACH common.customer WHERE 
            common.customer.brand EQ "1"        AND 
            common.customer.OrgId BEGINS "V00"  AND
            common.customer.CustIdType EQ "CIF" AND
           (common.customer.Category EQ "20" OR
            common.customer.Category EQ "")
       USE-INDEX Orgid:
            
       lcOldCategory = common.customer.Category.
       
       /* Changing data when NOT executed in simulation mode */
       IF NOT llSimul THEN 
           common.customer.Category = "30" NO-ERROR.
       
       PUT UNFORMATTED common.customer.CustNum  ";" 
                       common.customer.CustIdType  ";" 
                       common.customer.OrgId ";"
                       lcOldCategory ";" 
                       ERROR-STATUS:ERROR
                       SKIP.                                             
   END.

OUTPUT CLOSE.

MESSAGE "Execution finished." SKIP 
        "Please check file:" lcFile 
    VIEW-AS ALERT-BOX.
