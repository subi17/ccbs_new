
/* ----------------------------------------------------------------------
  MODULE .......: addlinedisccron.i 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: susanjee 
  CREATED ......: 13.09.17
  CHANGED ......:
  Version ......: ccbs
----------------------------------------------------------------------- */

/* Function checks for existing 3P convergent for a customer */
FUNCTION fCheckExistingConvergentForAddLine RETURNS LOGICAL
   (INPUT icCustIDType    AS CHAR,
    INPUT icCustID        AS CHAR,
    INPUT icCliType       AS CHAR,
    OUTPUT lcConvCLI      AS CHAR,
    OUTPUT lcConvCLIType  AS CHAR,
    OUTPUT lcOrderChannel AS CHAR,
    OUTPUT ldeCrStamp     AS DEC):

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.
   DEFINE BUFFER bClitype  FOR Clitype.
   DEFINE BUFFER bOrder    FOR Order.

   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Parameters:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Parameters:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.PayType = FALSE                   AND
            (bMobSub.MsStatus = {&MSSTATUS_ACTIVE}     OR
             bMobSub.MsStatus = {&MSSTATUS_BARRED}),
       FIRST bCliType WHERE bCliType.Brand = Syst.Parameters:gcBrand AND bCliType.CliType = bMobSub.CliType NO-LOCK:

      IF bCliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
          NEXT.

      IF fIsConvergentAddLineOK(bMobSub.CLIType,icCliType) THEN DO:

         FIND FIRST bOrder NO-LOCK WHERE
                    bOrder.MsSeq   = bMobSub.MsSeq   AND
                    bOrder.CLIType = bMobSub.CLIType NO-ERROR.

         IF AVAIL bOrder THEN
            ASSIGN lcConvCLI      = bMobSub.CLI
                   lcConvCLIType  = bMobSub.CLIType
                   lcOrderChannel = bOrder.OrderChannel
                   ldeCrStamp     = bOrder.CrStamp.

         RETURN TRUE.
      END.

   END.

   RETURN FALSE.

END FUNCTION.

/* Function checks for existing Mobile Only for a customer
   Additional Line with mobile only ALFMO-5 */
FUNCTION fCheckExistingMobileOnlyForAddLine RETURNS LOGICAL
   (INPUT icCustIDType    AS CHAR,
    INPUT icCustID        AS CHAR,
    INPUT icCliType       AS CHAR,
    OUTPUT lcMOCLI        AS CHAR,
    OUTPUT lcMOCLIType    AS CHAR,
    OUTPUT lcOrderChannel AS CHAR,
    OUTPUT ldeCrStamp     AS DEC):

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.
   DEFINE BUFFER bOrder    FOR Order.

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Parameters:gcBrand AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(icCliType, {&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.

   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Parameters:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Parameters:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.PayType = FALSE:

       /* This is to handle where the additional line
          is CONT25 or CONT26 because it can treat itself
          as main line */
       IF (bMobSub.CLIType = ENTRY(3,{&ADDLINE_CLITYPES} ) OR
           bMobSub.CLIType = ENTRY(4,{&ADDLINE_CLITYPES} )) AND
          CAN-FIND(FIRST DPMember WHERE
                         DPMember.DPId      = DiscountPlan.DPId     AND
                         DPMember.HostTable = "MobSub"              AND
                         DPMember.KeyValue  = STRING(bMobSub.MsSeq) AND
                         DPMember.ValidTo  >= TODAY)                THEN NEXT.

       IF fIsMobileOnlyAddLineOK(bMobSub.CLIType,icCliType) THEN DO:
          FIND FIRST bOrder NO-LOCK WHERE
                     bOrder.MsSeq = bMobSub.MsSeq NO-ERROR.

          IF AVAIL bOrder THEN
             ASSIGN lcMOCLI        = bMobSub.CLI
                    lcMOCLIType    = bMobSub.CLIType 
                    lcOrderChannel = bOrder.OrderChannel
                    ldeCrStamp     = bOrder.CrStamp.

          RETURN TRUE.
       END.

   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckAvailDiscount RETURNS LOGICAL
   (INPUT icKeyValue AS CHAR,
    INPUT icCLIType  AS CHAR): 

   DEF BUFFER bDiscountPlan FOR DiscountPlan.
   DEF BUFFER bDPMember     FOR DPMember.

   DEF VAR lcDiscount AS CHAR NO-UNDO.  
   DEF VAR liCount    AS INT  NO-UNDO.

   CASE icCLIType:
      WHEN "CONT10" THEN lcDiscount = "DISCCONT10H,DISCCONT10HM".
      WHEN "CONT15" THEN lcDiscount = "DISCCONT15H,DISCCONT15HM".
      WHEN "CONT25" THEN lcDiscount = "DISCCONT25H,DISCCONT25HM".
      WHEN "CONT26" THEN lcDiscount = "DISCCONT26H,DISCCONT26HM".
   END CASE.  

   DO liCount = 1 TO NUM-ENTRIES(lcDiscount):
      
      FIND FIRST bDiscountPlan NO-LOCK WHERE 
                 bDiscountPlan.Brand    = Syst.Parameters:gcBrand   AND
                 bDiscountPlan.DPRuleId = ENTRY(liCount,lcDiscount) AND 
                 bDiscountPlan.ValidTo >= TODAY                     NO-ERROR.

      IF NOT AVAIL bDiscountPlan THEN NEXT.

      FIND FIRST bDPMember NO-LOCK WHERE 
                 bDPMember.DPId       = bDiscountPlan.DPId AND 
                 bDPMember.HostTable  = "MobSub"           AND 
                 bDPMember.KeyValue   = icKeyValue         AND 
                 bDPMember.ValidTo   >= TODAY              AND 
                 bDPMember.ValidFrom <= bDPMember.ValidTo  NO-ERROR.

      IF AVAIL bDPMember THEN 
         RETURN TRUE.

   END.

   RETURN FALSE.

END FUNCTION.

