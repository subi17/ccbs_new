/* ----------------------------------------------------------------------
  MODULE .......: fixedlinefunc.i
  TASK .........: Functions for handling fixed line related functionality
                  Reference Convergent offer project YPR-4729.                 
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 19.11.15
  CHANGED ......:
  ------------------------------------------------------------------------*/
&IF "{&FIXEDLINEFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE FIXEDLINEFUNC_I YES

{Func/matrix.i}
{Syst/tmsconst.i}
{Func/fcustpl.i}

/*Function returns True if a tariff can be defined as convergent tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fIsConvergenceTariff RETURNS LOGICAL
   (icCliType AS CHAR):

   DEF BUFFER CLIType FOR CLIType.

   FIND FIRST CLIType NO-LOCK WHERE
              CLIType.Brand EQ Syst.Var:gcBrand AND
              CLIType.CliType EQ icCLIType NO-ERROR.
   IF AVAIL CliType AND
      CliType.FixedLineDownload NE ? AND 
      CliType.FixedLineDownload NE "" THEN RETURN TRUE.
   
   RETURN FALSE.
END FUNCTION.


/*Used when there is no clitype available directly.*/
/*Function returns True if a tariff can be defined as convergent tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fHasConvergenceTariff RETURNS LOGICAL
   (iiMsSeq AS INT):
   DEF BUFFER bCLIType FOR CLIType.
   DEF BUFFER bMobsub FOR MobSub.
   DEF BUFFER bTermMS FOR TermMobSub.
   DEF VAR lcCliType AS CHAR.

   FIND FIRST bMobsub NO-LOCK WHERE
              bMobSub.MsSeq EQ iiMsSeq NO-ERROR.
   IF NOT AVAIL bMobSub THEN DO:
      FIND FIRST bTermMS NO-LOCK WHERE 
                 bTermMS.MsSeq EQ iiMsSeq NO-ERROR.
      IF NOT AVAIL bTermMS THEN RETURN FALSE.
      ELSE lcCliType = bTermMS.CLIType.
   END.
   ELSE lcCLIType = bMobSub.CliType.

   RETURN fIsConvergenceTariff(lcCLIType).
END FUNCTION.

FUNCTION fCanTerminateConvergenceTariff RETURNS LOGICAL
   (iiMsSeq AS INT,
    iiTerminationReason AS INT,
    OUTPUT ocError AS CHAR):

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderFusion FOR OrderFusion.

   IF iiTerminationReason EQ ? OR
      iiTerminationReason EQ {&SUBSCRIPTION_TERM_REASON_MNP}
      THEN RETURN TRUE.

   FOR EACH Order NO-LOCK WHERE
            Order.MsSeq = iiMsSeq AND
            LOOKUP(Order.StatusCode, {&ORDER_INACTIVE_STATUSES}) = 0,
      FIRST OrderFusion NO-LOCK WHERE
            OrderFusion.Brand = Syst.Var:gcBrand AND
            OrderFusion.OrderID = Order.OrderID:

      IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN DO:

         IF Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:
            ocError = "Ongoing Convergent order".
            RETURN FALSE.
         END.
         ELSE ocError = "Warning: Ongoing convergent order".
   
      END.
      ELSE DO:
      
         IF Order.StatusCode NE {&ORDER_STATUS_PENDING_MOBILE_LINE} AND
            Order.StatusCode NE {&ORDER_STATUS_MNP_REJECTED} THEN DO:
            ocError = "Mobile line order is ongoing".
            RETURN FALSE.
         END.
         ELSE ocError = "Warning: Mobile line order is ongoing".
   
      END.
   END.

   RETURN TRUE.

END FUNCTION.

/* Check if convergent contract based by subscription name.
   All convergent subscription name starts CONTDSL (ADSL) or 
   CONTFH (fiber)*/
FUNCTION fIsConvergentFixedContract RETURNS LOGICAL
   (icContract AS CHAR):
   IF icContract BEGINS "CONTDSL" OR
      icContract BEGINS "CONTFH" THEN
      RETURN TRUE.
   /* SVAs are assosiated with fixed line and should not be
      terminated until fixedline terminated */
   IF icContract EQ "FAXTOEMAIL" OR
      icContract EQ "OFFICE365" OR
      icContract EQ "SAGEONE" OR
      icContract EQ "IPFIJA" OR
      icContract EQ "Centralita" OR
      icContract BEGINS "FTERM" THEN
      RETURN TRUE.
   IF INDEX(icContract,"FIX") > 0 THEN
      RETURN TRUE.

   RETURN FALSE.
END FUNCTION.

/* Check if Convergent tariff OR FixedOnly tariff */ 
FUNCTION fIsConvergentORFixedOnly RETURNS LOGICAL
   (icCLIType AS CHARACTER):

   DEFINE BUFFER bCLIType FOR CLIType.
   
   IF CAN-FIND(FIRST bCLIType NO-LOCK WHERE
                     bCLIType.Brand      = Syst.Var:gcBrand           AND
                     bCLIType.CLIType    = icCLIType                         AND
                    (bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT}  OR 
                     bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY})) THEN 
      RETURN TRUE.

   RETURN FALSE.

END FUNCTION.

/* Function returns True if a tariff can be defined as fixedonly (2P) tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fIsFixedOnly RETURNS LOGICAL
   (icCLIType AS CHARACTER):

   DEFINE BUFFER bCLIType FOR CLIType.
   
   IF CAN-FIND(FIRST bCLIType NO-LOCK WHERE
                     bCLIType.Brand      = Syst.Var:gcBrand           AND
                     bCLIType.CLIType    = icCLIType                         AND
                     bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY}) THEN 
      RETURN TRUE.

   RETURN FALSE.

END FUNCTION.

/* Check if Convergent 3P tariff */
FUNCTION fIsConvergent3POnly RETURNS LOGICAL
   (icCLIType AS CHARACTER):

   DEFINE BUFFER bCLIType FOR CLIType.

   IF CAN-FIND(FIRST bCLIType NO-LOCK WHERE
                     bCLIType.Brand      = Syst.Var:gcBrand           AND
                     bCLIType.CLIType    = icCLIType                         AND
                     bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT})  THEN
      RETURN TRUE.
   RETURN FALSE.
END.

/* Check if Convergent tariff OR FixedOnly tariff */ 
FUNCTION fIsConvergentAddLineOK RETURNS LOGICAL
   (icCLITypeConv    AS CHARACTER,
    icCLITypeAddLine AS CHARACTER):

   DEF VAR lcResult AS CHAR NO-UNDO.

   DEF BUFFER bCLIType FOR CLIType.
   
   IF CAN-FIND(FIRST bCLIType NO-LOCK WHERE
                     bCLIType.Brand      = Syst.Var:gcBrand           AND
                     bCLIType.CLIType    = icCLITypeConv                     AND
                     bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT}) THEN DO:
      
      IF fMatrixAnalyse(Syst.Var:gcBrand,
                        "ADDLINE",
                        "SubsTypeFrom;SubsTypeTo",
                        icCLITypeConv + ";" + icCLITypeAddLine,
                        OUTPUT lcResult) = 1 THEN
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Check convergent STC compability. Special handling that allows convergent
   STC between subscription types which have same fixed line part.
   Convergent ADSL subscription can be changed to other ADSL
   Convergent fiber 50MB can be changed to other convergent 50MB fiber.
   Convergent fiber 300MB can be changed to other convergent 300MB fiber.
*/
FUNCTION fCheckConvergentSTCCompability RETURNS LOGICAL
   (INPUT icOldCliType AS CHAR,
    INPUT icNewCliType AS CHAR):
   DEF BUFFER bOldClitype FOR Clitype.

   /* compatible if both have same download speed for fixedline */
   FIND FIRST bOldClitype WHERE
              bOldClitype.brand EQ Syst.Var:gcBrand AND
              bOldClitype.clitype EQ icOldCliType NO-ERROR.
   IF AVAIL bOldClitype THEN
      IF CAN-FIND (FIRST Clitype WHERE
                         Clitype.brand EQ Syst.Var:gcBrand AND
                         Clitype.clitype EQ icNewCliType AND
                         Clitype.FixedLineDownload EQ 
                            bOldClitype.FixedLineDownload) THEN
         RETURN TRUE.
   
   /* otherwise is not compatible */
   RETURN FALSE.
END FUNCTION.                                        

/* Function checks for ongoing 3P convergent for a customer  */
FUNCTION fCheckOngoingConvergentOrder RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR): 

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.
   DEFINE BUFFER bClitype       FOR Clitype.

   FOR EACH bOrderCustomer NO-LOCK WHERE   
            bOrderCustomer.Brand      EQ Syst.Var:gcBrand AND 
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Var:gcBrand AND
            bOrder.orderid    EQ bOrderCustomer.Orderid  AND
            LOOKUP(bOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0,
      FIRST bOrderFusion NO-LOCK WHERE
            bOrderFusion.Brand   = Syst.Var:gcBrand AND
            bOrderFusion.OrderID = bOrder.OrderID,
      FIRST bCliType WHERE bCliType.Brand = Syst.Var:gcBrand AND bCliType.CliType = bOrder.CliType NO-LOCK:
      
      IF bCliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN 
          NEXT.

      IF fIsConvergentAddLineOK(bOrder.CLIType,icCliType) THEN 
         RETURN TRUE.

   END.

   RETURN FALSE.

END FUNCTION.

/* Function checks for ongoing pro migration for a customer  */
FUNCTION fCheckOngoingProMigration RETURNS LOGICAL
   (INPUT iiCustNum AS INT):

   DEFINE BUFFER OrderCustomer FOR OrderCustomer.
   DEFINE BUFFER Order         FOR Order.
   DEFINE BUFFER OrderFusion   FOR OrderFusion.
   DEFINE BUFFER Clitype       FOR Clitype.
   DEFINE BUFFER Customer       FOR Customer.
   DEFINE BUFFER CustCat        FOR CustCat.

   FOR FIRST Customer WHERE Customer.CustNum = iiCustNum NO-LOCK,
       EACH CustCat WHERE 
            CustCat.Brand EQ Syst.Var:gcBrand AND 
            CustCat.Category EQ Customer.Category AND 
            CustCat.Pro EQ False NO-LOCK, 
       EACH OrderCustomer WHERE 
            OrderCustomer.Brand EQ Syst.Var:gcBrand AND 
            OrderCustomer.CustIdType EQ Customer.CustIdType     AND
            OrderCustomer.CustId     EQ Customer.OrgId          AND
            OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} AND 
            OrderCustomer.Pro EQ TRUE NO-LOCK,
       EACH Order WHERE 
            Order.Brand EQ Syst.Var:gcBrand AND 
            Order.orderid EQ OrderCustomer.Orderid AND
            LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0,
      FIRST OrderFusion WHERE 
            OrderFusion.Brand EQ Syst.Var:gcBrand AND 
            OrderFusion.OrderID EQ Order.OrderID NO-LOCK:

      IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN 
         NEXT.    

      RETURN TRUE.

   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckOngoingNonProMigration RETURNS LOGICAL
   (INPUT iiCustNum AS INT):

   DEFINE BUFFER OrderCustomer FOR OrderCustomer.
   DEFINE BUFFER Order         FOR Order.
   DEFINE BUFFER Customer      FOR Customer.
   DEFINE BUFFER CustCat       FOR CustCat.

   FOR FIRST Customer WHERE Customer.CustNum = iiCustNum NO-LOCK,
       EACH CustCat WHERE 
            CustCat.Brand = Syst.Var:gcBrand AND 
            CustCat.Category = Customer.Category AND 
            CustCat.Pro = True NO-LOCK, 
       EACH OrderCustomer WHERE 
            OrderCustomer.Brand = Syst.Var:gcBrand AND 
            OrderCustomer.CustIdType = Customer.CustIdType AND 
            OrderCustomer.CustId = Customer.OrgID AND 
            OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} AND 
            OrderCustomer.Pro = False NO-LOCK,
       EACH Order NO-LOCK WHERE 
            Order.Brand EQ Syst.Var:gcBrand AND 
            Order.orderid EQ OrderCustomer.Orderid:

      IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN NEXT.

      IF Order.OrderType NE {&ORDER_TYPE_NEW} OR
         Order.OrderType NE {&ORDER_TYPE_MNP} OR
         Order.OrderType NE {&ORDER_TYPE_STC} THEN NEXT.

      RETURN TRUE.

   END.

   RETURN FALSE.

END FUNCTION.

/* Function checks for ongoing 3P convergent for a customer  */
FUNCTION fCheckOngoingConvergentOrderWithoutALCheck RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR):

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.
   DEFINE BUFFER bClitype       FOR Clitype.

   FOR EACH bOrderCustomer NO-LOCK WHERE
            bOrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Var:gcBrand AND
            bOrder.orderid    EQ bOrderCustomer.Orderid  AND
            bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL},
      FIRST bOrderFusion NO-LOCK WHERE
            bOrderFusion.Brand   = Syst.Var:gcBrand AND
            bOrderFusion.OrderID = bOrder.OrderID,
      FIRST bCliType WHERE bCliType.Brand = Syst.Var:gcBrand AND bCliType.CliType = bOrder.CliType NO-LOCK:

      IF bCliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
          NEXT.

      IF LOOKUP(bOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN NEXT.    

      RETURN TRUE.

   END.

   RETURN FALSE.

END FUNCTION.

/* Function checks for ongoing 2P convergent for a customer */
FUNCTION fCheckOngoing2PConvergentOrder RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR):

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.

   FOR EACH bOrderCustomer NO-LOCK WHERE
            bOrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Var:gcBrand AND
            bOrder.orderid    EQ bOrderCustomer.Orderid  AND
            bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE},
      FIRST bOrderFusion NO-LOCK WHERE
            bOrderFusion.Brand   = Syst.Var:gcBrand AND
            bOrderFusion.OrderID = bOrder.OrderID:

      IF fIsConvergentORFixedOnly(bOrder.CLIType) THEN
         RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Function checks for existing 3P convergent for a customer */
FUNCTION fCheckExistingConvergent RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR):

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

      IF fIsConvergentAddLineOK(bMobSub.CLIType,icCliType) THEN 
         RETURN TRUE.

   END.   

   RETURN FALSE.

END FUNCTION.

/* Function checks for existing 3P convergent for a customer 
   without checking additional line matrix support */
FUNCTION fCheckExistingConvergentWithoutALCheck RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR):

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
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.


/* Function checks for existing 2P OR 3P convergent for a customer */
FUNCTION fCheckExisting2PConvergent RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR):

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.

   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Var:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Var:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.PayType = FALSE:

      IF fIsConvergentORFixedOnly(bMobSub.CLIType) THEN
         RETURN TRUE.

   END.

   RETURN FALSE.

END FUNCTION.


FUNCTION fIsProSubscription RETURNS LOGICAL
   (INPUT iiMsSeq   AS INT):

   DEFINE BUFFER bMobSub   FOR MobSub.
   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bCustCat  FOR CustCat.

   FIND FIRST bMobSub WHERE bMobSub.MsSeq = iiMsSeq AND bMobSub.PayType = FALSE NO-LOCK NO-ERROR.
   IF NOT AVAIL bMobSub THEN RETURN FALSE.

   FIND FIRST bCustomer WHERE bCustomer.CustNum = bMobSub.Custnum NO-LOCK NO-ERROR.
   IF AVAIL bCustomer THEN 
   DO:
       FIND FIRST bCustCat WHERE bCustCat.Brand = Syst.Var:gcBrand AND bCustCat.Category = bCustomer.Category AND bCustCat.Pro = TRUE NO-LOCK NO-ERROR.
       IF AVAIL bCustCat THEN 
           RETURN TRUE.    
   END.        

   RETURN FALSE.
       
END FUNCTION.

/* Additional Line with mobile only ALFMO-5  */ 
FUNCTION fIsMobileOnlyAddLineOK RETURNS LOGICAL
   (icCLIType    AS CHARACTER,
    icCLITypeAddLine AS CHARACTER):

   DEF VAR lcResult AS CHAR NO-UNDO.

   DEF BUFFER bCLIType FOR CLIType.
   
   IF CAN-FIND(FIRST bCLIType NO-LOCK WHERE
               bCLIType.Brand      = Syst.Var:gcBrand           AND
               bCLIType.CLIType    = icCLIType                         AND
               bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY}) THEN DO:
      
      IF fMatrixAnalyse(Syst.Var:gcBrand,
                        "ADDLINEHM",
                        "SubsTypeFrom;SubsTypeTo",
                        icCLIType + ";" + icCLITypeAddLine,
                        OUTPUT lcResult) = 1 THEN
         RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Function checks for existing Mobile Only for a customer 
   Additional Line with mobile only ALFMO-5 */
FUNCTION fCheckExistingMobileOnly RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR):

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Var:gcBrand AND
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

       IF fIsMobileOnlyAddLineOK(bMobSub.CLIType,icCliType) THEN
          RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Function checks for ongoing mobile only for a customer 
   Additional Line with mobile only ALFMO-5  */
FUNCTION fCheckOngoingMobileOnly RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR):

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.

   FOR EACH bOrderCustomer NO-LOCK WHERE
            bOrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Var:gcBrand AND
            bOrder.orderid    EQ bOrderCustomer.Orderid  AND
            bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL}   AND
            LOOKUP(bOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0:

       IF CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                   OrderAction.Brand    = Syst.Var:gcBrand AND
                   OrderAction.OrderID  = bOrder.OrderID   AND
                   OrderAction.ItemType = "AddLineDiscount" AND
          LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS_HM}) > 0 ) THEN
             NEXT.

      IF fIsMobileOnlyAddLineOK(bOrder.CLIType,icCliType) THEN
         RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Return true if tarif belongs to convergent additional line */
FUNCTION fIsAddLineTariff RETURNS LOGICAL
   (INPUT icCli AS CHAR):
   DEFINE BUFFER bMobSub FOR MobSub.
   DEFINE BUFFER bDiscountPlan FOR DiscountPlan.
   DEFINE BUFFER bDPMember FOR DPMember.

   FOR FIRST bMobSub NO-LOCK WHERE
             bMobSub.Brand = Syst.Var:gcBrand AND
             bMobSub.CLI   = icCli AND
      LOOKUP(bMobSub.CliType, {&ADDLINE_CLITYPES}) > 0,
         EACH bDiscountPlan NO-LOCK WHERE
              bDiscountPlan.Brand  = Syst.Var:gcBrand AND
       LOOKUP(bDiscountPlan.DPRuleID, {&ADDLINE_DISCOUNTS} + ","
                                   + {&ADDLINE_DISCOUNTS_20} + ","
                                   + {&ADDLINE_DISCOUNTS_HM}) > 0 AND
              bDiscountPlan.ValidTo >= TODAY,
         FIRST bDPMember NO-LOCK WHERE
               bDPMember.DPID       = bDiscountPlan.DPID AND
               bDPMember.HostTable  = "MobSub" AND
               bDPMember.KeyValue   = STRING(bMobSub.MsSeq) AND
               bDPMember.ValidTo   >= TODAY AND
               bDPMember.ValidFrom <= bDPMember.ValidTo:

         RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Function selects correct permanency amount for an order */
FUNCTION fSelectFTERMFee RETURNS CHAR
   (INPUT iiOrderID  AS INT,
    OUTPUT odValue   AS DEC,
    OUTPUT ocFeeName AS CHAR):

   odValue = 0.0.
   ocFeeName = "".

   FIND FIRST OrderAction NO-LOCK WHERE
              OrderAction.Brand EQ Syst.Var:gcBrand AND
              OrderAction.Orderid EQ iiOrderId AND
              OrderAction.itemkey BEGINS "fterm" NO-ERROR.
   IF NOT AVAIL OrderAction THEN RETURN "No FTERM orderaction".

   FIND FIRST DayCampaign NO-LOCK WHERE
              DayCampaign.brand EQ Syst.Var:gcBrand AND
              DayCampaign.dcevent eq OrderAction.ItemKey NO-ERROR.
   IF NOT AVAIL DayCampaign THEN RETURN "No FTERM dayycampaign".

   FIND FIRST FMItem NO-LOCK WHERE
              FMItem.Brand  EQ DayCampaign.Brand AND
              FMItem.FeeModel EQ DayCampaign.TermFeeModel NO-ERROR.
   IF NOT AVAIL FMItem THEN RETURN "No FTERM fmitem".

   odValue = FMItem.Amount.
   ocFeeName = FMItem.FeeModel.
   RETURN "".
END.



/* Return true if order made for convergent additional line */
FUNCTION fIsAddLineOrder RETURNS LOGICAL
   (INPUT iiOrderId AS INT):
   DEFINE BUFFER bOrderAction FOR OrderAction.

   FOR FIRST bOrderAction NO-LOCK WHERE
             bOrderAction.Brand = Syst.Var:gcBrand AND
             bOrderAction.OrderId   = iiOrderId AND
             bOrderAction.ItemType = "AddLineDiscount":
         
      RETURN TRUE.
   END.
   
   RETURN FALSE.

END FUNCTION.

/* If main line is installed for Main line (Convergent order or Mobile only)
   then don't move additional line order to 76 */
FUNCTION fCheckFixedLineStatusForMainLine RETURNS LOGICAL
   (INPUT icCustIDType   AS CHAR,
    INPUT icCustID       AS CHAR,
    INPUT icCliType      AS CHAR):

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.
   DEFINE BUFFER bCLIType       FOR CLIType.

   FOR EACH bOrderCustomer NO-LOCK WHERE
            bOrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Var:gcBrand AND
            bOrder.OrderId    EQ bOrderCustomer.OrderId  AND
            bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL},
      FIRST bOrderFusion NO-LOCK WHERE
            bOrderFusion.Brand        EQ Syst.Var:gcBrand AND
            bOrderFusion.OrderID      EQ bOrder.OrderID          AND 
            bOrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_FINALIZED}, 
      FIRST bCLIType WHERE bCLIType.Brand   EQ Syst.Var:gcBrand AND 
                           bCLIType.CliType EQ bOrder.CLIType          NO-LOCK:

      IF NOT fIsConvergenceTariff(bCLIType.CLIType) THEN NEXT. 

      IF fIsConvergentAddLineOK(bOrder.CLIType,icCliType) THEN 
         RETURN TRUE.

   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fGetMobileLineCompareFee RETURNS DECIMAL
    (icCLIType    AS CHARACTER,
     icBaseBundle AS CHARACTER,
     idaActivated AS DATE ):

    DEF BUFFER bMobileLine    FOR CliType.
    DEF BUFFER bDayCampaign   FOR DayCampaign.
    DEF BUFFER bFMItem        FOR FMItem.
    DEF BUFFER bCliType       FOR CliType.

    DEF VAR lcFMPriceList             AS CHAR NO-UNDO.
    DEF VAR ldeActivatedTS            AS DECI NO-UNDO.
    DEF VAR ldeFee                    AS DECI NO-UNDO.

    FIND FIRST bMobileLine WHERE
               bMobileLine.Brand   = Syst.Var:gcBrand AND
               bMobileLine.CLIType = icBaseBundle     NO-LOCK NO-ERROR.
    IF AVAIL bMobileLine THEN 
        ASSIGN ldeFee = bMobileLine.CompareFee.
    ELSE
    DO:
        FIND FIRST bDayCampaign WHERE bDayCampaign.Brand   = Syst.Var:gcBrand AND 
                                      bDayCampaign.DCEvent = icBaseBundle     NO-LOCK NO-ERROR.
        IF AVAIL bDayCampaign AND bDayCampaign.FeeModel <> "" THEN 
        DO:
            ASSIGN ldeActivatedTS = Func.Common:mDate2TS(idaActivated).

            FIND FIRST bCliType WHERE
                       bCliType.Brand   = Syst.Var:gcBrand AND
                       bCliType.CLIType = icCliType        NO-LOCK NO-ERROR.
            IF AVAIL bCliType THEN
            DO:
                FOR EACH PListConf USE-INDEX RatePlan NO-LOCK WHERE
                         PListConf.Brand    = Syst.Var:gcBrand   AND
                         PListConf.RatePlan = bCliType.PricePlan AND
                         PListConf.dFrom   <= idaActivated       AND
                         PListConf.dTo     >= idaActivated,
                   FIRST PriceList OF PListConf NO-LOCK,
                   FIRST bFMItem NO-LOCK WHERE
                         bFMItem.Brand     = Syst.Var:gcBrand      AND
                         bFMItem.FeeModel  = bDayCampaign.FeeModel AND
                         bFMItem.PriceList = PriceList.PriceList   AND 
                         bFMitem.FromDate  <= idaActivated         AND
                         bFMitem.ToDate    >= idaActivated                  
                   BY PListConf.Prior:
                   lcFMPriceList = PListConf.PriceList.
                   LEAVE.
                END.
            END.

            IF lcFMPriceList = "" THEN 
                ASSIGN lcFMPriceList = "COMMON".

            FIND FIRST bFMItem WHERE
                       bFMItem.Brand     = Syst.Var:gcBrand        AND
                       bFMItem.FeeModel  = bDayCampaign.FeeModel   AND
                       bFMItem.PriceList = lcFMPriceList           AND
                       bFMItem.FromDate <= idaActivated            AND
                       bFMItem.ToDate   >= idaActivated            NO-LOCK NO-ERROR.
            IF AVAIL bFMItem THEN 
                ASSIGN ldeFee = bFMItem.Amount.  
        END.
        ELSE 
            ASSIGN ldeFee = 0.  
    END.

    RETURN ldeFee.

END FUNCTION.

FUNCTION fSendFixedLineTermReqToMuleDB RETURNS CHAR
   ( INPUT iiOrderId AS INTEGER ):

   DEFINE VARIABLE lcUriPath        AS CHARACTER       NO-UNDO.
   DEFINE VARIABLE objRESTClient    AS CLASS Gwy.ParamRESTClient.
   DEFINE VARIABLE loOMParser       AS CLASS Progress.Json.ObjectModel.ObjectModelParser NO-UNDO.
   DEFINE VARIABLE loJsonConstruct  AS CLASS Progress.Json.ObjectModel.JsonConstruct     NO-UNDO.
   DEFINE VARIABLE lii              AS INTEGER         NO-UNDO.
   DEFINE VARIABLE lcError          AS CHARACTER       NO-UNDO.
   DEFINE VARIABLE liMuleESBIFInUse AS INT             NO-UNDO.

   liMuleESBIFInUse = Syst.Parameters:geti("TerminatioNotificationAPIInUse", "RESTMuleESB").
   IF liMuleESBIFInUse EQ 0 THEN
      RETURN "".

   DO ON ERROR UNDO, THROW:

      objRESTClient = NEW Gwy.ParamRESTClient("RESTMuleESB").
      objRESTClient:mSetURIPath(SUBSTITUTE("api/orders/1/Order/Y&1/TerminateLandline",iiOrderId)).

      objRESTClient:mPOST().

      CATCH loRESTError AS Gwy.RESTError:

         /* NOTE: The errors automatically are logged to the client log */

         IF loRESTError:ErrorMessage > ""
         THEN DO ON ERROR UNDO, THROW:
            ASSIGN
               loOMParser      = NEW Progress.Json.ObjectModel.ObjectModelParser()
               loJsonConstruct = loOMParser:Parse(loRESTError:ErrorMessage).

            IF TYPE-OF(loJsonConstruct, Progress.Json.ObjectModel.JsonObject)
               THEN RETURN CAST(loJsonConstruct, Progress.Json.ObjectModel.JsonObject):GetCharacter("resultDescription").
            ELSE RETURN STRING(SUBSTRING(loRESTError:ErrorMessage, 1, 30000)).

            CATCH loError AS Progress.Lang.Error:
               RETURN STRING(SUBSTRING(loRESTError:ErrorMessage, 1, 30000)).
            END CATCH.

            FINALLY:
               IF VALID-OBJECT(loOMParser)
                  THEN DELETE OBJECT loOMParser.
            END FINALLY.
         END.

         IF loRESTError:ReturnValue > ""
            THEN RETURN loRESTError:ReturnValue.
   
         DO lii = 1 TO loRESTError:NumMessages:
            lcError = lcError + "," + loRESTError:GetMessage(lii).
         END.

         IF lcError > ""
            THEN RETURN LEFT-TRIM(lcError,",").
   
         RETURN "Error was thrown but no error message available".

      END CATCH.
     
      FINALLY:
         IF VALID-OBJECT(objRESTClient)
            THEN DELETE OBJECT objRESTClient.
      END FINALLY.

   END.
   RETURN "".

END FUNCTION.


FUNCTION fFindFixedLineOrder RETURNS INTEGER
   ( iiMsSeq AS INTEGER ):

   DEFINE BUFFER Order      FOR Order.
   DEFINE BUFFER MobSub     FOR MobSub.
   DEFINE BUFFER bActionLog FOR ActionLog.

   DEF VAR liMsSeq AS INT NO-UNDO.

   FIND FIRST Mobsub NO-LOCK USE-INDEX MsSeq WHERE
              Mobsub.MsSeq       EQ iiMsSeq AND
              Mobsub.FixedNumber GT ""      NO-ERROR.

   IF NOT AVAILABLE MobSub THEN 
      RETURN 0.

   /* Check if terminated subscription is Merged 3P subscription */
   FIND FIRST bActionLog NO-LOCK  WHERE
              bActionLog.Brand     EQ Syst.Var:gcBrand     AND
              bActionLog.TableName EQ "MobSub"             AND
              bActionLog.KeyValue  EQ STRING(MobSub.MsSeq) AND
              bActionLog.ActionID  EQ {&MERGE2P3P}         NO-ERROR.

   IF AVAIL bActionLog THEN
      liMsSeq = INT(ENTRY(1,bActionLog.ActionChar,CHR(255))).
   ELSE liMsSeq = iiMsSeq.

   FOR EACH Order NO-LOCK WHERE 
            Order.MsSeq EQ liMSSeq BY Order.CrStamp DESC:

      IF NOT CAN-FIND(FIRST OrderFusion NO-LOCK USE-INDEX OrderId WHERE
                            OrderFusion.Brand        EQ "1"                AND
                            OrderFusion.OrderId      EQ Order.OrderId      AND
                            OrderFusion.FixedNumber  EQ Mobsub.FixedNumber AND
                            OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_FINALIZED})
      THEN NEXT.

      RETURN Order.OrderId.

   END.

   RETURN 0.

END FUNCTION.

&ENDIF
