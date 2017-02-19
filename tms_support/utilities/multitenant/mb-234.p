/*  Otsikko

*/

{Func/multitenantfunc.i}
DEF BUFFER bRegion FOR Region.
DEF BUFFER bInvGroup FOR InvGroup.
DEF BUFFER bCustomer FOR Customer.
DEF BUFFER bTaxzone FOR TaxZone.
DEF VAR lgsimulate AS LOG NO-UNDO INIT FALSE.

fsetEffectiveTenantForAllDB("Default").
FIND FIRST customer NO-LOCK where
           customer.custnum = 300 no-error.
IF NOT AVAIL customer then
   run ../tms_support/utilities/multitenant/import_yoigo_template_customers.p.

fsetEffectiveTenantForAllDB("TMasMovil").

def buffer bvatcode for vatcode.

DEFINE VARIABLE liCode AS INT NO-UNDO. 

FIND FIRST vatcode NO-LOCK where
           vatcode.vatcode >= 50 no-error.
IF NOT AVAIL vatcode then
FOR EACH vatcode NO-LOCK where
         vatcode.todate > today:

   case vatcode.vatcode:
      when 5  then liCode = 50.
      when 6  then liCode = 51.
      when 12  then liCode = 60.
      when 13  then liCode = 61.
      when 21  then liCode = 70.
      when 22  then liCode = 71.
      when 30  then liCode = 80.
      when 31  then liCode = 81.
      when 40  then liCode = 90.
      otherwise next.
   end.

   create bvatcode.
   assign
      bvatcode.vatcode = liCode
      bvatcode.taxzone = string(int(vatcode.taxzone) + 5)
      bvatcode.accnum = vatcode.accnum. /* change to correct */
   buffer-copy vatcode except vatcode taxzone accnum to bvatcode.
end.


FIND FIRST Region
NO-ERROR.  /* Regions are already in fixtures? */
IF NOT AVAIL Region THEN DO:
   for each Region  
      tenant-where buffer-tenant-name(region) NE "TMasMovil":
      IF lgSimulate THEN
         DISP Region.
      ELSE DO:
         fsetEffectiveTenantForAllDB("TMasMovil").
         CREATE bRegion.
         BUFFER-COPY Region to bRegion.
         bregion.taxzone = STRING(INT(bregion.taxzone) + 5).      
      END.
   end.

   /* Can this deleted from Yoigo table? Moved to not used taxzone 9.
      Taxzone 5 will be used by Masmovil */
      
END.

/* Check if masmovil data is already made taxzones > 6 */
FIND FIRST InvGroup WHERE 
           INVGroup.taxzone EQ "10" NO-ERROR.
IF NOT AVAIL InvGroup THEN DO:
   /* tax zone 5 needed for prepaid topups ? 
   FIND FIRST InvGroup WHERE
              INVGroup.taxzone EQ "5" NO-ERROR.
   IF AVAIL InvGroup THEN
      IF lgSimulate THEN
         DISP invgroup.
      ELSE DO:
         InvGroup.taxzone = "9".
      END.
   */

   FOR EACH InvGroup WHERE
            INT(InvGroup.taxzone) GE 1 AND
            INT(InvGroup.taxzone) LE 5:
      IF lgSimulate THEN
         DISP InvGroup.
      ELSE DO:
         CREATE bInvGroup.
         IF InvGroup.invGroup EQ "VAT1" THEN DO:
            bInvGroup.invGroup = "VAT2".
            bInvGroup.invform = "VAT2".
            bInvGroup.IGName = REPLACE(bInvGroup.IGName, " 1", " 2").
         END.
         ELSE IF InvGroup.invGroup EQ "IGIC1" THEN DO:
            bInvGroup.invGroup = "IGIC2".
            bInvGroup.invform = "IGIC2".
            bInvGroup.IGName = REPLACE(bInvGroup.IGName, " 1", " 2").
         END.
         ELSE IF InvGroup.invGroup EQ "IPSIC1" THEN DO:
            bInvGroup.invGroup = "IPSIC2".
            bInvGroup.invform = "IPSIC2".
            bInvGroup.IGName = REPLACE(bInvGroup.IGName, " 1", " 2").
         END.
         ELSE IF InvGroup.invGroup EQ "IPSIM1" THEN DO:
            bInvGroup.invGroup = "IPSIM2".
            bInvGroup.invform = "IPSIM2".
            bInvGroup.IGName = REPLACE(bInvGroup.IGName, " 1", " 2").
         END.
         ELSE IF InvGroup.invGroup EQ "YOIGO" THEN DO:
            bInvGroup.invGroup = "MASMOVIL".
            bInvGroup.invform = "MASMOVIL".
            binvgroup.IGName = REPLACE(invgroup.IGName,"Yoigo","MasMovil").
         END.
         ELSE DO:
            /*MESSAGE "Incorrect invGroup " + bInvGroup.invGroup
            VIEW-AS ALERT-BOX.   */
            NEXT.
         END.      
         MESSAGE "invGroup " + InvGroup.invGroup VIEW-AS ALERT-BOX.
         

         BUFFER-COPY InvGroup EXCEPT taxzone invgroup invform IgName TO bInvGroup.
         bInvGroup.taxzone = STRING(INT(InvGroup.taxzone) + 5).
        
      END.
   END.
END.

FIND FIRST TaxZone WHERE 
           Taxzone.taxzone EQ "10" NO-ERROR.
IF NOT AVAIL TAXZone THEN DO:
   FOR EACH TaxZone WHERE
            INT(Taxzone.taxzone) < 6:
      CREATE bTaxzone.
      bTaxzone.taxzone = STRING(INT(Taxzone.taxzone) + 5).
      IF Taxzone.taxzone EQ "5" THEN 
         bTaxzone.tzname = "MasMovil (own usage)".
      ELSE bTaxzone.tzname = Taxzone.tzname.
   END.
END.

/* Check if masmovil tenant already includes template customers */
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
         MESSAGE "Creating template customer " + STRING(Customer.custnum) 
         VIEW-AS ALERT-BOX.
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
ELSE MESSAGE "Customers already found" VIEW-AS ALERT-BOX.

/* MB-94 set imsi ranges for masmovil and yoigo for ICC loading check */
fsetEffectiveTenantForAllDB("TMasMovil").
FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.paramgroup EQ "SIM" AND
           TMSParam.paramcode EQ "IMSI_Begin" NO-ERROR.
   IF AVAIL TMSParam THEN tmsparam.charval="2140420".

fsetEffectiveTenantForAllDB("Default").
FIND FIRST TMSParam WHERE
           TMSParam.brand EQ "1" AND
           TMSParam.paramgroup EQ "SIM" AND
           TMSParam.paramcode EQ "IMSI_Begin" NO-ERROR.
   IF AVAIL TMSParam THEN tmsparam.charval="2140401".

/* MB-156 remove SMS invoice from MasMovil tenant */
fsetEffectiveTenantForAllDB("TMasMovil").
FIND FIRST TMSCodes WHERE 
           tmscodes.fieldname EQ "deltype" AND
           tmscodes.codename EQ "SMS" NO-ERROR.
IF AVAIL TMSCodes THEN   
   DELETE tmscodes.



