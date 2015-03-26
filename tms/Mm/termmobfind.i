CASE icType:

   WHEN "AGREEMENT" THEN 
      FIND {1} TermMobsub WHERE 
               TermMobsub.Brand   = gcBrand AND 
               TermMobsub.AgrCust = iiCustNum 
               {2}
      USE-INDEX AgrCust NO-LOCK NO-ERROR.         
      
   WHEN "INVOICE" THEN
      FIND {1} TermMobsub WHERE
               TermMobsub.Brand   = gcBrand AND 
               TermMobsub.InvCust = iiCustNum 
               {2}
      USE-INDEX InvCust NO-LOCK NO-ERROR.

   WHEN "USER" THEN DO:
      FIND {1} TermMobsub WHERE
               TermMobsub.Brand   = gcBrand AND 
               TermMobsub.CustNum = iiCustNum  
               {2}
      USE-INDEX CustNum NO-LOCK NO-ERROR.

   END.    
   OTHERWISE FIND {1} TermMobsub WHERE
                  TRUE 
                  {2}
   USE-INDEX cli NO-LOCK NO-ERROR.

END CASE.

