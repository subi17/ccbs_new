FUNCTION fCheckExistingMobileOnlySubscription RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR,
    INPUT iiALMsSeq    AS INT):

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand    = Syst.Var:gcBrand  AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(icCliType, {&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.

   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Var:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Var:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.PayType = FALSE:

       /* This is to handle where the additional line
          is CONT25 or CONT26 because it can treat itself
          as main line */  
       IF (bMobSub.CLIType = ENTRY(3,{&ADDLINE_CLITYPES} ) OR 
           bMobSub.CLIType = ENTRY(4,{&ADDLINE_CLITYPES} )) AND 
          CAN-FIND(FIRST DPMember WHERE
                         DPMember.DPId = DiscountPlan.DPId AND
                         DPMember.HostTable = "MobSub" AND
                         DPMember.KeyValue  = STRING(bMobSub.MsSeq) AND
                         DPMember.ValidTo   >= TODAY) THEN NEXT.

       IF fIsMobileOnlyAddLineOK(bMobSub.CLIType,icCliType) AND 
          bMobSub.MsSeq NE iiALMsSeq                        THEN
          RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckExistingConvergentSubscription RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR,
    INPUT iiALMsSeq    AS INT):

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.
   DEFINE BUFFER bClitype       FOR Clitype.

   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Var:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Var:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.PayType = FALSE                   AND
            (bMobSub.MsStatus = {&MSSTATUS_ACTIVE}     OR
             bMobSub.MsStatus = {&MSSTATUS_BARRED}),
       FIRST bCliType WHERE bCliType.Brand = Syst.Var:gcBrand AND bCliType.CliType = bMobSub.CliType NO-LOCK:
      
      IF bCliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN 
          NEXT.

      IF fIsConvergentAddLineOK(bMobSub.CLIType,icCliType) AND 
         bMobSub.MsSeq NE iiALMsSeq                        THEN 
         RETURN TRUE.

   END.   

   RETURN FALSE.

END FUNCTION.
