&IF "{&fsapc_i}" NE "YES" 
&THEN

&GLOBAL-DEFINE fsapc_i YES

{Func/cparam2.i}

FUNCTION fCustomerSAPC RETURNS LOGICAL
   (iiCustNum AS INTEGER):

   IF iiCustNum > 0 AND
      CAN-FIND(FIRST Customer NO-LOCK WHERE
                     Customer.CustNum EQ MsRequest.CustNum AND
                     Customer.AccGrp EQ 2)
   THEN RETURN TRUE.
   
   RETURN fCParam("SAPC", "SAPC_ENABLED_NEW_CUSTOMERS") EQ "SAPC".

END FUNCTION.

&ENDIF
