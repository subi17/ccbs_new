/*  Otsikko

*/

{Func/multitenantfunc.i}
DEF BUFFER bRegion FOR Region.
DEF BUFFER bInvGroup FOR InvGroup.
DEF BUFFER bCustomer FOR Customer.
DEF VAR lgsimulate AS LOG NO-UNDO INIT FALSE.

fsetEffectiveTenantForAllDB("TMasMovil").
FIND FIRST Region
NO-ERROR.
IF NOT AVAIL Region THEN DO:
   for each Region  
      tenant-where buffer-tenant-name(region) = "default":
      IF lgSimulate THEN
         DISP Region.
      ELSE DO:
         fsetEffectiveTenantForAllDB("TMasMovil").
         CREATE bRegion.
         BUFFER-COPY Region to bRegion.
         bregion.taxzone = STRING(INT(bregion.taxzone) + 4).      
      END.
   end.

   /* Can this deleted from Yoigo table? Moved to not used taxzone 9.
      Taxzone 5 will be used by Masmovil */
      
END.
ELSE DO:
   MESSAGE "MasMovil data found " Region.region VIEW-AS ALERT-BOX.

/*
   FOR EACH Region
      tenant-where buffer-tenant-name(Region) = "default" or 
                   buffer-tenant-name(Region) = "tmasmovil":
      DISP Region.
      DISP BUFFER-TENANT-NAME(Region).
   END.
*/
END.

FIND FIRST InvGroup WHERE 
           INVGroup.taxzone EQ "6" NO-ERROR.
IF NOT AVAIL InvGroup THEN DO:
   FIND FIRST InvGroup WHERE
              INVGroup.taxzone EQ "5" NO-ERROR.
   IF AVAIL InvGroup THEN
      IF lgSimulate THEN
         DISP invgroup.
      ELSE DO:
         InvGroup.taxzone = "9".
      END.
   FOR EACH InvGroup WHERE
            INT(InvGroup.taxzone) GE 1 AND
            INT(InvGroup.taxzone) LE 4:
      IF lgSimulate THEN
         DISP InvGroup.
      ELSE DO:
         CREATE bInvGroup.
         IF InvGroup.invGroup EQ "VAT1" THEN DO:
            bInvGroup.invGroup = "VAT2".
            bInvGroup.invform = "VAT2".
         END.
         ELSE IF InvGroup.invGroup EQ "IGIC1" THEN DO:
            bInvGroup.invGroup = "IGIC2".
            bInvGroup.invform = "IGIC2".
         END.
         ELSE IF InvGroup.invGroup EQ "IPSIC1" THEN DO:
            bInvGroup.invGroup = "IPSIC2".
            bInvGroup.invform = "IPSIC2".
         END.
         ELSE IF InvGroup.invGroup EQ "IPSIM1" THEN DO:
            bInvGroup.invGroup = "IPSIM2".
            bInvGroup.invform = "IPSIM2".
         END.
         ELSE DO:
            /*MESSAGE "Incorrect invGroup " + bInvGroup.invGroup
            VIEW-AS ALERT-BOX.   */
            NEXT.
         END.      
         MESSAGE "invGroup " + InvGroup.invGroup VIEW-AS ALERT-BOX.
         

         BUFFER-COPY InvGroup EXCEPT taxzone invgroup invform TO bInvGroup.
         bInvGroup.taxzone = STRING(INT(InvGroup.taxzone) + 4).
        
      END.
   END.
END.

fsetEffectiveTenantForAllDB("TMasMovil").
FIND FIRST Customer WHERE 
           Customer.custnum EQ 300 NO-ERROR.
IF NOT AVAIL Customer THEN DO:
   FOR EACH Customer WHERE
            Customer.custnum GE 300 AND
            Customer.custnum LE 303
            TENANT-WHERE buffer-tenant-name(Customer) NE "TMasMovil":
      IF lgSimulate THEN
         DISP Customer.
      ELSE DO:
         MESSAGE "aa " VIEW-AS ALERT-BOX.
         fsetEffectiveTenantForAllDB("TMasMovil").
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
ELSE MESSAGE "Cust found" VIEW-AS ALERT-BOX.

/* MB-94 set imsi ranges */
fsetEffectiveTenantForAllDB("TMasMovil").
FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.paramgroup EQ "SIM" AND
           TMSParam.paramcode EQ "IMSI_Begin".
   tmsparam.charval="2140120".

fsetEffectiveTenantForAllDB("Default").
FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.paramgroup EQ "SIM" AND
           TMSParam.paramcode EQ "IMSI_Begin".
   tmsparam.charval="2140401".

