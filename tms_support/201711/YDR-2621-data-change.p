   
   FOR EACH common.customer WHERE 
            common.customer.brand EQ "1"        AND 
            common.customer.OrgId BEGINS "V00"  AND
            common.customer.CustIdType EQ "CIF" AND
           (common.customer.Category EQ "20" OR
            common.customer.Category EQ "22")
       USE-INDEX Orgid:
       disp custnum.      
      end. 
