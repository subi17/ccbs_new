/* custfind.i

   changed:         01.10.03/aam Salesman and Reseller removed
                    25.04.05/aam use-index AgrCust with liAgrCust
                    14.12.05/aam invcust & users
                    16.12.05 jp  Inputcust
                    16.03.06/aam use msowner-check in "invcust"
*/

CASE lctyyppi:

   WHEN "brand" THEN 
      FIND {1} Customer WHERE 
               Customer.Brand  = lcBrand 
               {3}
      USE-INDEX {2} NO-LOCK NO-ERROR.         
      
   WHEN "invcust" THEN
      FIND {1} Customer WHERE
               Customer.Brand   = lcBrand     AND
               Customer.AgrCust = liMainCust  AND 
               CAN-FIND(FIRST MsOwner WHERE 
                              MsOwner.InvCust = Customer.CustNum)
               {3}
      USE-INDEX AgrCust NO-LOCK NO-ERROR.

   WHEN "inputcust" THEN
      FIND {1} Customer WHERE
               Customer.Brand   = lcBrand    AND
               Customer.CustNum = iiCustNum  
               {3}
      USE-INDEX CustNum NO-LOCK NO-ERROR.

   WHEN "agrusers" THEN 
      FIND {1} Customer WHERE
               Customer.Brand   = lcBrand     AND
               Customer.AgrCust = liMainCust  AND 
               CAN-FIND(FIRST MsOwner WHERE 
                              MsOwner.CustNum = Customer.CustNum)
               {3}
      USE-INDEX AgrCust NO-LOCK NO-ERROR.

   WHEN "invusers" THEN 
      FIND {1} Customer WHERE
               Customer.Brand   = lcBrand     AND
               Customer.InvCust = liMainCust  AND
               CAN-FIND(FIRST MsOwner WHERE 
                              MsOwner.CustNum = Customer.CustNum)
               {3}
      USE-INDEX InvCust NO-LOCK NO-ERROR.
       
   OTHERWISE FIND {1} Customer WHERE
                  TRUE 
                  {3}
   USE-INDEX CustNum_s NO-LOCK NO-ERROR.

END CASE.

