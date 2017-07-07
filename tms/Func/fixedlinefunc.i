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
{Syst/tmsconst.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Func/create_eventlog.i}
{Func/matrix.i}
/* Function makes new MSOwner when subscription is partially
   terminated or mobile part order closed. Calling program must have
   commali.i, katun defined and call fCleanEventObjects after this function */
FUNCTION fUpdatePartialMSOwner RETURNS LOGICAL
   (iiMsSeq AS INT,
    icFixedNumber AS CHAR):
   DEF VAR ldUpdateTS AS DEC NO-UNDO.
   DEF BUFFER MsOwner FOR MsOwner.
   DEF BUFFER bNewMsowner FOR Msowner.

   ldUpdateTS = fMakeTS().
   FIND FIRST MSOwner WHERE 
              MSOwner.MsSeq  = iiMsSeq AND
              MSOwner.TsEnd >= ldUpdateTS
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL MSOwner THEN RETURN FALSE.

   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhMsOwner AS HANDLE NO-UNDO.
      lhMsOwner = BUFFER MSOwner:HANDLE.
      RUN StarEventInitialize(lhMsOwner).
      RUN StarEventSetOldBuffer (lhMsOwner).
   END.

   MSOwner.TsEnd = ldUpdateTS.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent (lhMsOwner).
   END.

   CREATE bNewMsowner.
   BUFFER-COPY MSOwner EXCEPT TsEnd tsbegin TO bNewMsowner.
   ASSIGN
      bNewMsowner.CLI = icFixedNumber
      bNewMsowner.imsi = ""
      bNewMsowner.CliEvent = "F"
      bNewMsowner.tsbegin = fSecOffSet(ldUpdateTS,1)
      bNewMsowner.TsEnd = 99999999.99999.

   IF llDoEvent THEN DO:
      lhMsOwner = BUFFER bNewMsowner:HANDLE.
      fMakeCreateEvent (lhMsOwner, "", "", "").
   END.

   RELEASE MSOwner.
   RELEASE bNewMsowner.
   RETURN TRUE.

END.   


/*Function returns True if a tariff can be defined as convergent tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fIsConvergenceTariff RETURNS LOGICAL
   (icCliType AS CHAR):

   DEF BUFFER CLIType FOR CLIType.

   FIND FIRST CLIType NO-LOCK WHERE
              CLIType.Brand EQ Syst.Parameters:gcBrand AND
              CLIType.CliType EQ icCLIType NO-ERROR.
   IF AVAIL CliType AND
      CliType.FixedLineDownload NE ? AND 
      CliType.FixedLineDownload NE "" THEN RETURN TRUE.
   
   RETURN FALSE.
END.   


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
END.   

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
            OrderFusion.Brand = Syst.Parameters:gcBrand AND
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

END.

/* Check if convergent contract based by subscription name.
   All convergent subscription name starts CONTDSL (ADSL) or 
   CONTFH (fiber)*/
FUNCTION fIsConvergentFixedContract RETURNS LOGICAL
   (icContract AS CHAR):
   IF icContract BEGINS "CONTDSL" OR
      icContract BEGINS "CONTFH" THEN 
      RETURN TRUE.
   RETURN FALSE.
END.   

/* Check if Convergent tariff OR FixedOnly tariff */ 
FUNCTION fIsConvergentORFixedOnly RETURNS LOGICAL
   (icCLIType AS CHARACTER):

   DEFINE BUFFER bCLIType FOR CLIType.
   
   IF CAN-FIND(FIRST bCLIType NO-LOCK WHERE
                     bCLIType.Brand      = Syst.Parameters:gcBrand           AND
                     bCLIType.CLIType    = icCLIType                         AND
                     bCLIType.LineType   = {&CLITYPE_LINETYPE_MAIN}          AND 
                    (bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT}  OR 
                     bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY})) THEN 
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
                     bCLIType.Brand      = Syst.Parameters:gcBrand           AND
                     bCLIType.CLIType    = icCLITypeConv                     AND
                     bCLIType.LineType   = {&CLITYPE_LINETYPE_MAIN}          AND 
                     bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT}) THEN DO:
      
      IF fMatrixAnalyse(Syst.Parameters:gcBrand,
                        "ADDLINE",
                        "SubsTypeFrom;SubsTypeTo",
                        icCLITypeConv + ";" + icCLITypeAddLine,
                        OUTPUT lcResult) = 1 THEN
      RETURN TRUE.
   END.

   RETURN FALSE.

END.   


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
              bOldClitype.brand EQ Syst.Parameters:gcBrand AND
              bOldClitype.clitype EQ icOldCliType NO-ERROR.
   IF AVAIL bOldClitype THEN
      IF CAN-FIND (FIRST Clitype WHERE
                         Clitype.brand EQ Syst.Parameters:gcBrand AND
                         Clitype.clitype EQ icNewCliType AND
                         Clitype.FixedLineDownload EQ 
                            bOldClitype.FixedLineDownload) THEN
         RETURN TRUE.
   
   /* otherwise is not compatible */
   RETURN FALSE.
END.                                         

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
            bOrderCustomer.Brand      EQ Syst.Parameters:gcBrand AND 
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Parameters:gcBrand AND
            bOrder.orderid    EQ bOrderCustomer.Orderid  AND
            bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL}   AND 
           (bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} OR
            bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_MOBILE_LINE}),
      FIRST bOrderFusion NO-LOCK WHERE
            bOrderFusion.Brand   = Syst.Parameters:gcBrand AND
            bOrderFusion.OrderID = bOrder.OrderID,
      FIRST bCliType WHERE bCliType.Brand = Syst.Parameters:gcBrand AND bCliType.CliType = bOrder.CliType NO-LOCK:

      IF bCliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN 
          NEXT.
          
      IF fIsConvergentAddLineOK(bOrder.CLIType,icCliType) THEN 
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
            bOrderCustomer.Brand      EQ Syst.Parameters:gcBrand AND
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Parameters:gcBrand AND
            bOrder.orderid    EQ bOrderCustomer.Orderid  AND
            bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL}   AND
           (bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} OR
            bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_MOBILE_LINE}),
      FIRST bOrderFusion NO-LOCK WHERE
            bOrderFusion.Brand   = Syst.Parameters:gcBrand AND
            bOrderFusion.OrderID = bOrder.OrderID,
      FIRST bCliType WHERE bCliType.Brand = Syst.Parameters:gcBrand AND bCliType.CliType = bOrder.CliType NO-LOCK:

      IF bCliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
          NEXT.

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
            bOrderCustomer.Brand      EQ Syst.Parameters:gcBrand AND
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Parameters:gcBrand AND
            bOrder.orderid    EQ bOrderCustomer.Orderid  AND
            bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL}   AND
            bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE},
      FIRST bOrderFusion NO-LOCK WHERE
            bOrderFusion.Brand   = Syst.Parameters:gcBrand AND
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
             bCustomer.Brand      = Syst.Parameters:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Parameters:gcBrand AND
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
   IF AVAIL bMobsub THEN 
   DO:
       FIND FIRST bCustomer WHERE bCustomer.CustNum = bMobSub.InvCust AND bCustomer.Roles <> "inactive" NO-LOCK NO-ERROR.
       IF AVAIL bCustomer THEN 
       DO:
           FIND FIRST bCustCat WHERE bCustCat.Brand = Syst.Parameters:gcBrand AND bCustCat.Category = bCustomer.Category AND bCustCat.Pro = TRUE NO-LOCK NO-ERROR.
           IF AVAIL bCustCat THEN 
               RETURN TRUE.    
       END.        
   END.    
END FUNCTION.
/* Additional Line with mobile only ALFMO-5  */ 
FUNCTION fIsMobileOnlyAddLineOK RETURNS LOGICAL
   (icCLIType    AS CHARACTER,
    icCLITypeAddLine AS CHARACTER):

   DEF VAR lcResult AS CHAR NO-UNDO.

   DEF BUFFER bCLIType FOR CLIType.
   
   IF CAN-FIND(FIRST bCLIType NO-LOCK WHERE
               bCLIType.Brand      = Syst.Parameters:gcBrand           AND
               bCLIType.CLIType    = icCLIType                         AND
               bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY}) THEN DO:
      
      IF fMatrixAnalyse(Syst.Parameters:gcBrand,
                        "ADDLINEHM",
                        "SubsTypeFrom;SubsTypeTo",
                        icCLIType + ";" + icCLITypeAddLine,
                        OUTPUT lcResult) = 1 THEN
         RETURN TRUE.
   END.

   RETURN FALSE.

END.

/* Function checks for existing Mobile Only for a customer 
   Additional Line with mobile only ALFMO-5 */
FUNCTION fCheckExistingMobileOnly RETURNS LOGICAL
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR,
    INPUT icCliType    AS CHAR):

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.

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
       IF CAN-FIND(FIRST DPMember WHERE
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
            bOrderCustomer.Brand      EQ Syst.Parameters:gcBrand AND
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Parameters:gcBrand AND
            bOrder.orderid    EQ bOrderCustomer.Orderid  AND
            bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL}   AND
            LOOKUP(bOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0:

       IF CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                   OrderAction.Brand    = Syst.Parameters:gcBrand AND
                   OrderAction.OrderID  = bOrder.OrderID   AND
                   OrderAction.ItemType = "AddLineDiscount" AND
          LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS_HM}) > 0 ) THEN
             NEXT.

      IF fIsMobileOnlyAddLineOK(bOrder.CLIType,icCliType) THEN
         RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

&ENDIF
