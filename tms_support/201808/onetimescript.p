   
FUNCTION fCreateMenuText RETURN CHARACTER (INPUT iiMenuNum AS INTEGER , INPUT icMenuText AS CHARACTER )  :

    IF NOT CAN-FIND(FIRST MenuText WHERE MenuText.MenuNum = iiMenuNum ) THEN 
    DO:
    
        CREATE MenuText.
        ASSIGN MenuText.MenuNum  = iiMenuNum
               MenuText.MenuText = icMenuText.
    
    END.
    ELSE MESSAGE SUBSTITUTE("Menu already exists for &1" , iiMenuNum) VIEW-AS ALERT-BOX.


END FUNCTION .


 fCreateMenuText(9860,"ORDER   PRODUCT"). 
 fCreateMenuText(9861,"PRODUCT PARAM'S"). 
 fCreateMenuText(9862,"MOBILE  SUBSCRIPTION"). 
 fCreateMenuText(9863,"FIXED   SUBSCRIPTION"). 
 fCreateMenuText(9864,"CUSTOMERACCOUNT"). 
 fCreateMenuText(9865,"BILLING ACCOUNT"). 
 fCreateMenuText(9866,"TARGET  CUSTOMER"). 
 
 
 
   
   