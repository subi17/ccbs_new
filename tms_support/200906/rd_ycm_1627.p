
 FIND Customer NO-LOCK WHERE 
      Customer.Brand = "1" AND
      Customer.OrgId = "46029197X" NO-ERROR.
 IF AVAIL Customer THEN DO:
     FIND CURRENT Customer EXCLUSIVE-LOCK NO-ERROR.
     ASSIGN Customer.Category = "40".
 END.
    
