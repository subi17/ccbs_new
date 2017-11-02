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
{Func/cparam2.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/create_eventlog.i}
{Func/matrix.i}

/* Function makes new MSOwner when subscription is partially
   terminated or mobile part order closed. Calling program must have
   commali.i, Syst.Var:katun defined and call fCleanEventObjects after this function */
FUNCTION fUpdatePartialMSOwner RETURNS LOGICAL
   (iiMsSeq AS INT,
    icFixedNumber AS CHAR):
   DEF VAR ldUpdateTS AS DEC NO-UNDO.
   DEF BUFFER MsOwner FOR MsOwner.
   DEF BUFFER bNewMsowner FOR Msowner.

   ldUpdateTS = Func.Common:mMakeTS().
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
      bNewMsowner.tsbegin = Func.Common:mSecOffSet(ldUpdateTS,1)
      bNewMsowner.TsEnd = 99999999.99999.

   IF llDoEvent THEN DO:
      lhMsOwner = BUFFER bNewMsowner:HANDLE.
      fMakeCreateEvent (lhMsOwner, "", "", "").
   END.

   RELEASE MSOwner.
   RELEASE bNewMsowner.
   RETURN TRUE.

END FUNCTION.


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
   (INPUT icCustIDType  AS CHAR,
    INPUT icCustID      AS CHAR,
    INPUT icCliType     AS CHAR,
    INPUT icAction      AS CHAR,
    OUTPUT lcConvOrders AS CHAR): 

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.
   DEFINE BUFFER bClitype       FOR Clitype.

   DEF VAR lcOngoingOrdersList AS CHAR NO-UNDO INITIAL "". 

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
      
      IF fIsConvergentAddLineOK(bOrder.CLIType,icCliType) THEN DO:
         
         CASE icAction:
            WHEN {&ONGOING_ORDER_AVAIL} THEN RETURN TRUE. 
            WHEN {&ONGOING_ORDER_LIST} THEN DO:
               IF lcOngoingOrdersList EQ "" THEN 
                  lcOngoingOrdersList =  STRING(bOrder.OrderId).
               ELSE    
                  lcOngoingOrdersList = lcOngoingOrdersList + "," + STRING(bOrder.OrderId). 
            END.
         END CASE.
      
      END.
   END.

   CASE icAction:
      WHEN {&ONGOING_ORDER_LIST} THEN DO: 
         IF lcOngoingOrdersList NE "" THEN DO: 
            lcConvOrders = lcOngoingOrdersList.   
            RETURN TRUE.
         END. 
      END.   
   END. 

   RETURN FALSE.

END FUNCTION.

/* Function checks for ongoing pro migration for a customer  */
FUNCTION fCheckOngoingProMigration RETURNS LOGICAL
   (INPUT iiCustNum AS INT):

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.
   DEFINE BUFFER bClitype       FOR Clitype.
   DEFINE BUFFER Customer       FOR Customer.
   DEFINE BUFFER CustCat        FOR CustCat.

   FOR FIRST Customer WHERE Customer.CustNum = iiCustNum NO-LOCK,
       EACH CustCat WHERE CustCat.Brand = Syst.Var:gcBrand AND CustCat.Category = Customer.Category AND CustCat.Pro = False NO-LOCK, 
       EACH bOrderCustomer WHERE bOrderCustomer.CustNum = iiCustNum AND bOrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} AND bOrderCustomer.Pro = TRUE NO-LOCK,
       EACH bOrder WHERE bOrder.Brand EQ Syst.Var:gcBrand AND bOrder.orderid EQ bOrderCustomer.Orderid AND bOrder.OrderType NE {&ORDER_TYPE_RENEWAL} NO-LOCK,
       FIRST bOrderFusion WHERE bOrderFusion.Brand = Syst.Var:gcBrand AND bOrderFusion.OrderID = bOrder.OrderID NO-LOCK,
       FIRST bCliType WHERE bCliType.Brand = Syst.Var:gcBrand AND bCliType.CliType = bOrder.CliType NO-LOCK:

      IF bCliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
          NEXT.

      IF LOOKUP(bOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN 
         NEXT.    

      RETURN TRUE.

   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckOngoingNonProMigration RETURNS LOGICAL
   (INPUT iiCustNum AS INT):

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.
   DEFINE BUFFER bClitype       FOR Clitype.
   DEFINE BUFFER Customer       FOR Customer.
   DEFINE BUFFER CustCat        FOR CustCat.

   FOR FIRST Customer WHERE Customer.CustNum = iiCustNum NO-LOCK,
       EACH CustCat WHERE CustCat.Brand = "1" AND CustCat.Category = Customer.Category AND CustCat.Pro = True NO-LOCK, 
       EACH bOrderCustomer WHERE bOrderCustomer.CustNum = iiCustNum AND bOrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} AND bOrderCustomer.Pro = False NO-LOCK,
       EACH bOrder WHERE bOrder.Brand EQ Syst.Var:gcBrand AND bOrder.orderid EQ bOrderCustomer.Orderid AND bOrder.OrderType NE {&ORDER_TYPE_RENEWAL} NO-LOCK:

      IF LOOKUP(bOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN 
         NEXT.    

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
   (INPUT icCustIDType  AS CHAR,
    INPUT icCustID      AS CHAR,
    INPUT icCliType     AS CHAR,
    INPUT icAction      AS CHAR,
    OUTPUT lcConvOrders AS CHAR):

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.

   DEF VAR lcOngoingOrdersList AS CHAR NO-UNDO INITIAL "".

   FOR EACH bOrderCustomer NO-LOCK WHERE
            bOrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            bOrderCustomer.CustId     EQ icCustID                AND
            bOrderCustomer.CustIdType EQ icCustIDType            AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Var:gcBrand AND
            bOrder.orderid    EQ bOrderCustomer.Orderid  AND
            bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL}   AND
            bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE},
      FIRST bOrderFusion NO-LOCK WHERE
            bOrderFusion.Brand   = Syst.Var:gcBrand AND
            bOrderFusion.OrderID = bOrder.OrderID:

      IF fIsConvergentORFixedOnly(bOrder.CLIType) THEN DO:
         
         CASE icAction:
            WHEN {&ONGOING_ORDER_AVAIL} THEN RETURN TRUE.
            WHEN {&ONGOING_ORDER_LIST} THEN DO:
               IF lcOngoingOrdersList EQ "" THEN
                  lcOngoingOrdersList =  STRING(bOrder.OrderId).
               ELSE
                  lcOngoingOrdersList = lcOngoingOrdersList + "," + STRING(bOrder.OrderId).
            END.
         END CASE.
 
      END.
   END.

   CASE icAction:
      WHEN {&ONGOING_ORDER_LIST} THEN DO:
         IF lcOngoingOrdersList NE "" THEN DO:
            lcConvOrders = lcOngoingOrdersList.
            RETURN TRUE.
         END.
      END.
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
   IF AVAIL bMobsub THEN 
   DO:
       FIND FIRST bCustomer WHERE bCustomer.CustNum = bMobSub.InvCust AND bCustomer.Roles <> "inactive" NO-LOCK NO-ERROR.
       IF AVAIL bCustomer THEN 
       DO:
           FIND FIRST bCustCat WHERE bCustCat.Brand = Syst.Var:gcBrand AND bCustCat.Category = bCustomer.Category AND bCustCat.Pro = TRUE NO-LOCK NO-ERROR.
           IF AVAIL bCustCat THEN 
               RETURN TRUE.    
       END.        
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
   (INPUT icCustIDType  AS CHAR,
    INPUT icCustID      AS CHAR,
    INPUT icCliType     AS CHAR,
    INPUT icAction      AS CHAR,
    OUTPUT lcConvOrders AS CHAR):

   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderFusion   FOR OrderFusion.

   DEF VAR lcOngoingOrdersList AS CHAR NO-UNDO INITIAL "". 

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

      IF fIsMobileOnlyAddLineOK(bOrder.CLIType,icCliType) THEN DO:
         
         CASE icAction:
            WHEN {&ONGOING_ORDER_AVAIL} THEN RETURN TRUE.
            WHEN {&ONGOING_ORDER_LIST} THEN DO:
               IF lcOngoingOrdersList EQ "" THEN
                  lcOngoingOrdersList =  STRING(bOrder.OrderId).
               ELSE
                  lcOngoingOrdersList = lcOngoingOrdersList + "," + STRING(bOrder.OrderId).
            END.
         END CASE.
      
      END.
   END.

   CASE icAction:
      WHEN {&ONGOING_ORDER_LIST} THEN DO:
         IF lcOngoingOrdersList NE "" THEN DO:
            lcConvOrders = lcOngoingOrdersList.
            RETURN TRUE.
         END.
      END.
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


FUNCTION fCheckExistingConvergentAvailForExtraLine RETURNS LOGICAL
   (INPUT icCustIDType       AS CHAR,
    INPUT icCustID           AS CHAR,
    OUTPUT liMainLineOrderId AS INT):

   DEFINE BUFFER Customer FOR Customer.
   DEFINE BUFFER MobSub   FOR MobSub.
   DEFINE BUFFER Order    FOR Order.

   DEF VAR lcExtraMainLineCLITypes AS CHAR NO-UNDO. 

   lcExtraMainLineCLITypes = fCParam("DiscountType","Extra_MainLine_CLITypes").

   FOR FIRST Customer WHERE
             Customer.Brand      = Syst.Var:gcBrand AND
             Customer.OrgId      = icCustID                AND
             Customer.CustidType = icCustIDType            AND
             Customer.Roles     NE "inactive"              NO-LOCK,
       EACH  MobSub NO-LOCK WHERE
             MobSub.Brand    = Syst.Var:gcBrand AND
             MobSub.CustNum  = Customer.CustNum        AND
             MobSub.PayType  = FALSE                   AND
            (MobSub.MsStatus = {&MSSTATUS_ACTIVE} OR
             MobSub.MsStatus = {&MSSTATUS_BARRED})     BY MobSub.ActivationTS:

       IF LOOKUP(MobSub.CLIType,lcExtraMainLineCLITypes) = 0 THEN NEXT.
       
       IF MobSub.MultiSimID  <> 0                       AND 
          MobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} THEN NEXT.

       FIND LAST Order NO-LOCK WHERE 
                 Order.MsSeq      = MobSub.MsSeq              AND 
                 Order.CLIType    = MobSub.CLIType            AND 
                 Order.StatusCode = {&ORDER_STATUS_DELIVERED} AND 
          LOOKUP(STRING(Order.OrderType),"0,1,4") > 0         NO-ERROR.
          IF NOT AVAIL Order THEN 
             FIND LAST Order NO-LOCK WHERE 
                       Order.MsSeq      = MobSub.MsSeq              AND 
                       Order.StatusCode = {&ORDER_STATUS_DELIVERED} AND 
                LOOKUP(STRING(Order.OrderType),"0,1,4") > 0         NO-ERROR.

       IF NOT AVAIL Order THEN NEXT.          

       liMainLineOrderId = Order.OrderId. 

       RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckOngoingConvergentAvailForExtraLine RETURNS LOGICAL
   (INPUT icCustIDType      AS CHAR,
    INPUT icCustID          AS CHAR,
    OUTPUT liOngoingOrderId AS INT):
   
   DEFINE BUFFER OrderCustomer FOR OrderCustomer.
   DEFINE BUFFER Order         FOR Order.
   DEFINE BUFFER OrderFusion   FOR OrderFusion.

   DEF VAR lcExtraMainLineCLITypes AS CHAR NO-UNDO.

   lcExtraMainLineCLITypes = fCParam("DiscountType","Extra_MainLine_CLITypes").

   FOR EACH OrderCustomer NO-LOCK WHERE
            OrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            OrderCustomer.CustId     EQ icCustID                AND
            OrderCustomer.CustIdType EQ icCustIDType            AND
            OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH Order NO-LOCK WHERE
            Order.Brand        EQ Syst.Var:gcBrand AND
            Order.orderid      EQ OrderCustomer.Orderid   AND
            Order.OrderType    NE {&ORDER_TYPE_RENEWAL}   AND
            Order.MultiSimId   EQ 0                       AND 
            Order.MultiSimType EQ 0,
      FIRST OrderFusion NO-LOCK WHERE
            OrderFusion.Brand   = Syst.Var:gcBrand AND
            OrderFusion.OrderID = Order.OrderID           BY Order.CrStamp:

      IF LOOKUP(Order.CLIType,lcExtraMainLineCLITypes) = 0 THEN NEXT.

      IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN NEXT.
 
      liOngoingOrderId = Order.OrderId.

      RETURN TRUE.
 
   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckFixedLineInstalledForMainLine RETURNS LOGICAL
   (INPUT liMainLineOrderId  AS INT,
    INPUT liExtraLineOrderId AS INT):

   DEFINE BUFFER Order FOR Order. 

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand        EQ Syst.Var:gcBrand    AND
              Order.OrderId      EQ liMainLineOrderId          AND
       LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 AND
              Order.MultiSimId   EQ liExtraLineOrderId         AND
              Order.MultiSimType EQ {&MULTISIMTYPE_PRIMARY}    AND 
              Order.OrderType    NE {&ORDER_TYPE_RENEWAL}      NO-ERROR.

   IF AVAIL Order THEN DO: 
     
      /* If Fixed line is installed for Main line Convergent Order 
         THEN dont move extra line order to 76 */
      FIND FIRST OrderFusion NO-LOCK WHERE
                 OrderFusion.Brand        = Syst.Var:gcBrand          AND
                 OrderFusion.OrderID      = Order.OrderID                    AND 
                 OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_FINALIZED} NO-ERROR.
                 
      IF AVAIL OrderFusion THEN 
         RETURN FALSE.

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

&ENDIF
