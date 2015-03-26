/* fcustdata.i        14.12.05/aam
                      16.11.06/aam fChkCustID for spanish IDs  
                      21.11.06/aam fDefInvGroup
                      15.02.07/aam cif check corrected
                      20.06.07 kl  CIF check corrected
   
*/

&IF "{&CustDataDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CustDataDef YES


/* customer's roles */
FUNCTION fCustRoles RETURNS CHARACTER
   (BUFFER ibRoleCust FOR Customer). 

   /* return string is 'agrcust/invcust/user', e.g. '110' */
   RETURN STRING((ibRoleCust.AgrCust = ibRoleCust.CustNum),"1/0") +
          STRING(CAN-FIND(FIRST Customer WHERE 
                                Customer.InvCust = ibRoleCust.CustNum),"1/0") +
          STRING(CAN-FIND(FIRST MsOwner WHERE
                                MsOwner.CustNum = ibRoleCust.CustNum),"1/0").

END FUNCTION.

FUNCTION fChkZipCode RETURNS LOGICAL
  (icZipCode AS CHAR,
   icCountry AS CHAR).

   DEF VAR liZip AS INT NO-UNDO.
   
   /* all zipcodes must be numeric */
   liZip = INTEGER(icZipCode) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN FALSE. 
   
   /* finnish codes must be 5 digits */
   IF icCountry = "FI" AND LENGTH(icZipCode) NE 5 THEN RETURN FALSE.
   
   RETURN TRUE.

END FUNCTION.

FUNCTION fChkCustID RETURNS LOGICAL
   (icCustIDType AS CHAR,
    icCustID     AS CHAR):
    
   DEF VAR liIDCnt AS INT  NO-UNDO.
   DEF VAR liIDRes AS INT  NO-UNDO.
   DEF VAR liIDSum AS INT  NO-UNDO.
   DEF VAR lcIDRes AS CHAR NO-UNDO.
   DEF VAR lcBegin AS CHAR NO-UNDO.

   CASE icCustIDType:
   /* no checks for unknown customers */
   WHEN "N/A" THEN RETURN TRUE.

   WHEN "CIF" THEN DO:
      IF LENGTH(icCustID) NE 9 THEN RETURN FALSE.

      IF INDEX("ABCDEFGHJKLMNPQRSUVW",SUBSTRING(icCustID,1,1)) = 0 THEN 
         RETURN FALSE.

      DO liIDCnt = 2 TO 8:
         IF INDEX("0123456789",SUBSTRING(icCustID,liIDCnt,1)) = 0 THEN
            RETURN FALSE.
      END.

      liIDSum = 0.
      
      DO liIDCnt = 2 TO 8 BY 2:
         liIDRes = INTEGER(SUBSTRING(icCustID,liIDCnt,1)) * 2.

         liIDSum = liIDSum + INTEGER(SUBSTRING(STRING(liIDres),1,1)).
         IF liIDres >= 11 THEN 
            liIDSum = liIDSum + INTEGER(SUBSTRING(STRING(liIDres),2,1)).
      END.

      DO liIDCnt = 3 TO 7 BY 2:
          liIDSum = liIDSum + INTEGER(SUBSTRING(icCustID,liIDCnt,1)).
      END.

      ASSIGN liIDRes = IF liIDSum >= 10 THEN 2 ELSE 1
             liIDSum = INTEGER(SUBSTRING(STRING(liIDSum),liIDRes,1))
             liIDCnt = 10 - INTEGER(liIDSum).
              
      IF liIDCnt = 10 THEN liIDCnt = 0.

      IF liIDCnt = 0 
      THEN lcIDRes = "J".
      ELSE lcIDRes = SUBSTRING("ABCDEFGHI",liIDCnt,1).
      
      /* YCM-1312: 
        New: if CIF first character is one of 
        A,B,C,D,E,F,G,H,U or V last digit must be between 0..9 
        and if CIF first character is one of
        K,L,M,P,Q,R,S,W last digit must be between A..J */
      /* Old: may be either letter or digit */
      IF INDEX("ABCDEFGHJUV", SUBSTRING(icCustId,1,1)) > 0 THEN
      DO:
         IF STRING(liIDCnt) NE SUBSTRING(icCustId,9,1) THEN RETURN FALSE.
      END.

      IF INDEX("KMLNPQRSW", SUBSTRING(icCustId,1,1)) > 0 THEN
      DO:
         IF lcIDRes NE SUBSTRING(icCustId,9,1) THEN RETURN FALSE.
      END.
   END.
   
   WHEN "NIF" THEN DO:

      IF LENGTH(icCustID) NE 9 THEN RETURN FALSE.
      
      DO liIDCnt = 1 TO 8:
         IF INDEX("0123456789",SUBSTRING(icCustID,liIDCnt,1)) = 0 THEN
            RETURN FALSE.
      END.

      liIDCnt = INTEGER(SUBSTRING(icCustID,1,8)) MOD 23 + 1.
        
      IF SUBSTRING("TRWAGMYFPDXBNJZSQVHLCKET",liIDCnt,1) NE 
         SUBSTRING(icCustID,9,1)
      THEN RETURN FALSE. 
   END. 

   WHEN "NIE" THEN DO:

      IF LENGTH(icCustID) < 9 OR LENGTH(icCustID) > 10 THEN RETURN FALSE.
      
      lcBegin = SUBSTRING(icCustID,1,1).
      
      IF LOOKUP(lcBegin,"X,Y") = 0 THEN RETURN FALSE. 
      icCustID = SUBSTRING(icCustID,2).
      IF lcBegin = "Y" THEN icCustID = "1" + icCustID.

      DO liIDCnt = 1 TO LENGTH(icCustID) - 1:
         IF INDEX("0123456789",SUBSTRING(icCustID,liIDCnt,1)) = 0 THEN
            RETURN FALSE.
      END.

      liIDCnt = INTEGER(SUBSTRING(icCustID,1,LENGTH(icCustID) - 1)) MOD 23 + 1.
        
      IF SUBSTRING("TRWAGMYFPDXBNJZSQVHLCKET",liIDCnt,1) NE 
         SUBSTRING(icCustID,LENGTH(icCustID),1)
      THEN RETURN FALSE. 
   END. 
    
   END CASE.
   
   RETURN TRUE.
       
END FUNCTION.

FUNCTION fDefInvGroup RETURNS CHARACTER
   (icRegion AS CHAR):
   
   IF icRegion > "" THEN DO:
      FIND Region WHERE Region.Region = icRegion NO-LOCK NO-ERROR.
      IF AVAILABLE Region THEN 
      FOR FIRST InvGroup NO-LOCK WHERE
                InvGroup.Brand   = gcBrand AND
                InvGroup.TaxZone = Region.TaxZone:
         RETURN InvGroup.InvGroup.       
      END.   
   END.

   /* if nothing was found based on region then get first with empty taxzone */
   FOR FIRST InvGroup NO-LOCK WHERE
             InvGroup.Brand   = gcBrand AND
             InvGroup.TaxZone = "":
      RETURN InvGroup.InvGroup.       
   END.   

   /* if nothing was found then just get first */
   FOR FIRST InvGroup NO-LOCK WHERE
             InvGroup.Brand   = gcBrand:
      RETURN InvGroup.InvGroup.       
   END.   

   RETURN "".  
   
END FUNCTION.

FUNCTION fGetMobsubLimit RETURNS INT 
   (INPUT piCustNum AS INTEGER,
    INPUT pcCustomerCategory AS CHAR,
    OUTPUT olIsDefaultLimit AS LOG):
  
   FIND FIRST Limit NO-LOCK WHERE
      Limit.custnum = piCustnum AND 
      Limit.todate >= TODAY AND
      Limit.fromdate <= TODAY AND
      Limit.limittype = 2 NO-ERROR.

   IF NOT AVAIL Limit OR Limit.LimitAmt = ? THEN DO:
      
      olIsDefaultLimit = YES.
      
      FIND Custcat WHERE
         Custcat.brand = gcBrand AND
         CustCat.category = pcCustomerCategory NO-LOCK NO-ERROR.
      IF AVAIL CustCat THEN
         RETURN CustCat.MobSubLimit.
      ELSE RETURN 0.

   END.
   
   olIsDefaultLimit = FALSE.
   RETURN INT(Limit.LimitAmt).

END.

FUNCTION fGetMobsubActLimit RETURNS INT 
   (INPUT piCustNum AS INTEGER,
    INPUT pcCustomerCategory AS CHAR,
    OUTPUT olIsDefaultActLimit AS LOG):
  
   FIND FIRST Limit NO-LOCK WHERE
      Limit.custnum = piCustnum AND 
      Limit.todate >= TODAY AND
      Limit.fromdate <= TODAY AND
      Limit.limittype = 4 NO-ERROR.

   IF NOT AVAIL Limit OR Limit.LimitAmt = ? THEN DO:
      
      olIsDefaultActLimit = YES.
      
      FIND Custcat WHERE
         Custcat.brand = gcBrand AND
         CustCat.category = pcCustomerCategory NO-LOCK NO-ERROR.
      IF AVAIL CustCat THEN
         RETURN CustCat.ActivationLimit.
      ELSE RETURN 0.

   END.
   
   olIsDefaultActLimit = FALSE.
   RETURN INT(Limit.LimitAmt).

END.

&ENDIF

