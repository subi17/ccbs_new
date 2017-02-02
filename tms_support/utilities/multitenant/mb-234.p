DEF BUFFER bRegion FOR Region.
DEF BUFFER bInvGroup FOR InvGroup.
DEF BUFFER bCustomer FOR Customer.
DEF VAR lgsimulate AS LOG NO-UNDO INIT TRUE.

SET-EFFECTIVE-TENANT ("tMasmovil", "common").
FIND FIRST Region
NO-ERROR.
IF NOT AVAIL Region THEN DO:
   for each Region  
      tenant-where buffer-tenant-name(region) = "default":
      SET-EFFECTIVE-TENANT ("tmasmovil", "common").
      IF lgSimulate THEN
         DISP Region.
      ELSE DO:
         CREATE bRegion.
         BUFFER-COPY Region to bRegion.
         bregion.taxzone = STRING(INT(bregion.taxzone) + 4).      
      END.
   end.

   /* Can this deleted from Yoigo table? Moved to not used taxzone 9.
      Taxzone 5 will be used by Masmovil */
   SET-EFFECTIVE-TENANT ("default", "common").
   FIND FIRST InvGroup WHERE
              INVGroup.taxzone EQ "5" NO-ERROR.
   IF AVAIL InvGroup THEN
      IF lgSimulate THEN
         DISP invgroup.
      ELSE DO: 
         InvGroup.taxzone = "9".
      END.
   FOR EACH InvGroup WHERE
            INT(InvGroup.taxzone) > 0 AND
            INT(InvGroup.taxzone) < 5
            tenant-where buffer-tenant-name(InvGroup) = "default":
      IF lgSimulate THEN
         DISP InvGroup.
      ELSE DO:
         SET-EFFECTIVE-TENANT ("tmasmovil", "common").
         CREATE bInvGroup.
         BUFFER-COPY InvGroup TO bInvGroup.
         bInvGroup.taxzone = STRING(INT(bInvGroup.taxzone) + 4).
      END.
   END. 
      
   FOR EACH Customer WHERE
            Customer.custnum GE 300 AND
            Customer.custnum LE 303 
            TENANT-WHERE buffer-tenant-name(Customer) = "default":
      IF lgSimulate THEN
         DISP Customer.
      ELSE DO:
         SET-EFFECTIVE-TENANT ("tmasmovil", "common").
         CREATE bCustomer.
         BUFFER-COPY Customer TO bCustomer.
         IF bCustomer.invGroup EQ "VAT1" THEN 
            bCustomer.invGroup = "VAT2".
         ELSE IF bCustomer.invGroup EQ "IGIC1" THEN
            bCustomer.invGroup = "IGIC2".
         ELSE IF bCustomer.invGroup EQ "IPSIC1" THEN
            bCustomer.invGroup = "IPSIC2".
         ELSE IF bCustomer.invGroup EQ "IPSIM1" THEN
            bCustomer.invGroup = "IPSIM2".
         ELSE
            MESSAGE "Incorrect invGroup " + bCustomer.invGroup 
            VIEW-AS ALERT-BOX.
      END.
   END.     
END.
ELSE DO:
   MESSAGE "MasMovil data found " Region.region VIEW-AS ALERT-BOX.


   FOR EACH Region
      tenant-where buffer-tenant-name(Region) = "default" or 
                   buffer-tenant-name(Region) = "tmasmovil":
      DISP Region.
      DISP BUFFER-TENANT-NAME(Region).
   END.
END.
