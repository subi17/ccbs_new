/* Search Subscription based on the mentioned criteria.
 *
 * Parameters are in the toplevel array and in two integers.
 * An array is described here:
 * @input subscription_type         string  - optional
          subscription_bundle_id    string  - optional
          data_bundle_id            string  - optional
          other_bundles             string  - optional
          segmentation_code         string  - optional
          payterm                   string  - optional
          term                      string  - optional          
          order_end_date            date    - optional
          order_status              boolean - optional
          order_type                string  - optional
          eligible_renewal          boolean - optional
          any_barring               boolean - optional
          debt                      boolean - optional
          pay_type                  boolean 
          usage_type                integer
          order_start_date          date
          person_id_type            integer
 * Integers are described here:
          offset                    integer - mandatory
          limit_of_subscriptions    integer - mandatory
  */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "Newton".
{Syst/tmsconst.i}
{Func/barrfunc.i}
{Mm/active_bundle.i}
{Func/fdss.i}
{Func/orderchk.i}

/* Input parameters */
DEF VAR pcCliType      AS CHAR NO-UNDO.
DEF VAR piSubsLimit    AS INT NO-UNDO.
DEF VAR piOffset       AS INT NO-UNDO.
DEF VAR pcSubsBundleId AS CHAR NO-UNDO.
DEF VAR pcDataBundleId AS CHAR NO-UNDO.
DEF VAR pcOtherBundles AS CHAR NO-UNDO.
DEF VAR pcSegmentOffer AS CHAR NO-UNDO.
DEF VAR pcPayTerm      AS CHAR NO-UNDO.
DEF VAR pcTerm         AS CHAR NO-UNDO.
DEF VAR pcStruct       AS CHAR NO-UNDO.
DEF VAR pdtEndDate     AS DATE NO-UNDO. 
DEF VAR pcOrderType    AS CHAR NO-UNDO.  
DEF VAR pcLanguage     AS CHAR NO-UNDO.
DEF VAR pcInvGroup     AS CHAR NO-UNDO.
DEF VAR plBarring      AS LOG  NO-UNDO.
DEF VAR plDebt         AS LOG  NO-UNDO.
DEF VAR plPayType      AS LOG  NO-UNDO. /* YDA-895 */
DEF VAR piUsageType    AS INT  NO-UNDO. /* YDA-895 */
DEF VAR pdtStartDate   AS DATE NO-UNDO. /* YDA-895 */ 
DEF VAR piPersonIdType AS INT  NO-UNDO. /* YDA-895 */

/* Local variables */
DEF VAR lcDataBundles  AS CHAR NO-UNDO.
DEF VAR resp_array     AS CHAR NO-UNDO.
DEF VAR top_struct     AS CHAR NO-UNDO.
DEF VAR resp_struct    AS CHAR NO-UNDO.
DEF VAR lcstruct       AS CHAR NO-UNDO.
DEF VAR lcOtherBundle  AS CHAR NO-UNDO.
DEF VAR lcSegmentCode  AS CHAR NO-UNDO.
DEF VAR liCount        AS INT  NO-UNDO.
DEF VAR liNumberOfBundles  AS INT  NO-UNDO.
DEF VAR liNumberOfSubs     AS INT  NO-UNDO INIT 0.
DEF VAR liNumberLimit      AS INT  NO-UNDO INIT 0.
DEF VAR lcBundleCLITypes   AS CHAR NO-UNDO.
DEF VAR ldtOrderDate       AS DATE NO-UNDO. 
DEF VAR liOrderTime        AS INT  NO-UNDO. 
DEF VAR liLoopBegTime      AS INT  NO-UNDO.
DEF VAR liLoopEndTime      AS INT  NO-UNDO. 
DEF VAR liLoopCount        AS INT  NO-UNDO. 
DEF VAR plgOrderStatus     AS LOG  NO-UNDO.
DEF VAR plgEligibleRenewal AS LOG  NO-UNDO INIT ?.
DEF VAR ldtFirstDay        AS DATE NO-UNDO.

DEF BUFFER bfMobSub FOR MobSub.

ASSIGN ldtFirstDay = DATE(MONTH(ADD-INTERVAL(TODAY,-12,"months") + 1),
                        1,YEAR(ADD-INTERVAL(TODAY,-12,"months") + 1)).      

IF validate_request(param_toplevel_id, "struct,int,int") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
lcstruct = validate_struct(pcStruct,
   "subscription_type,subscription_bundle_id,data_bundle_id,other_bundles,segmentation_code,payterm,term,order_end_date,order_status,order_type,eligible_renewal,language,invoice_group,any_barring,debt,pay_type,usage_type,order_start_date,person_id_type").

ASSIGN
   pcCliType      = get_string(pcStruct, "subscription_type")
      WHEN LOOKUP("subscription_type", lcStruct) > 0
   piOffset       = get_int(param_toplevel_id, "1")
   piSubsLimit    = get_int(param_toplevel_id, "2")
   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

ASSIGN
   pcSubsBundleId = get_string(pcStruct, "subscription_bundle_id")
      WHEN LOOKUP(pcCliType,lcBundleCLITypes) > 0
   pcDataBundleId = get_string(pcStruct, "data_bundle_id")
      WHEN LOOKUP("data_bundle_id", lcstruct) > 0
   pcOtherBundles = get_string(pcStruct, "other_bundles")
      WHEN LOOKUP("other_bundles", lcstruct) > 0
   pcSegmentOffer = get_string(pcStruct, "segmentation_code")
      WHEN LOOKUP("segmentation_code", lcStruct) > 0
   pcPayTerm      = get_string(pcStruct, "payterm")
      WHEN LOOKUP("payterm", lcStruct) > 0
   pcTerm         = get_string(pcStruct, "term")
      WHEN LOOKUP("term", lcStruct) > 0   
   pdtEndDate     = get_date(pcStruct, "order_end_date")
      WHEN LOOKUP("order_end_date", lcStruct) > 0
   plgOrderStatus = get_bool(pcStruct, "order_status")
      WHEN LOOKUP("order_status", lcStruct) > 0
   pcOrderType    = get_string(pcStruct, "order_type")
      WHEN LOOKUP("order_type", lcStruct) > 0
   pcLanguage     = get_string(pcStruct, "language")
      WHEN LOOKUP("language", lcStruct) > 0
   pcInvGroup     = get_string(pcStruct, "invoice_group")
      WHEN LOOKUP("invoice_group", lcStruct) > 0
   plgEligibleRenewal = get_bool(pcStruct,"eligible_renewal")
      WHEN LOOKUP("eligible_renewal", lcStruct) > 0
   plBarring      = get_bool(pcStruct,"any_barring")
      WHEN LOOKUP("any_barring", lcStruct) > 0
   plDebt         = get_bool(pcStruct,"debt")
      WHEN LOOKUP("debt", lcStruct) > 0
   plPayType      = get_bool(pcStruct,"pay_type") 
      WHEN LOOKUP("pay_type", lcStruct) > 0
   piUsageType    = get_int(pcStruct,"usage_type") 
      WHEN LOOKUP("usage_type", lcStruct) > 0
   pdtStartDate   = get_date(pcStruct, "order_start_date")
      WHEN LOOKUP("order_start_date", lcStruct) > 0
   piPersonIdType = get_int(pcStruct, "person_id_type")
      WHEN LOOKUP("person_id_type", lcStruct) > 0. 
    /* Paytype, usagetype, startdate and personidtype YDA-895 */

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pdtEndDate > TODAY THEN 
   pdtEndDate = ?.

/* YDA-895 To narrow down the list */
IF pdtStartDate > TODAY THEN 
   pdtStartDate = ?.

liNumberOfBundles = NUM-ENTRIES(pcOtherBundles).

/* Add main array */
ASSIGN top_struct = add_struct(response_toplevel_id, "")
       resp_array = add_array(top_struct, "mobsubs").

liLoopBegTime = TIME.

FUNCTION fCheckEligibleRenewal RETURNS LOGICAL ():
   
   DEF VAR llBarrings           AS LOGICAL   NO-UNDO.
   DEF VAR lcBarrStatus         AS CHARACTER NO-UNDO.   
   DEF VAR ldaLastTerminal      AS DATE      NO-UNDO INIT ?.
   DEF VAR llCancelledPrerenove AS LOGICAL   NO-UNDO.
   DEF VAR llPrerenove          AS LOGICAL NO-UNDO INIT FALSE.
   DEF VAR liTime               AS INTEGER NO-UNDO.
   DEF VAR liConfigDays         AS INTEGER NO-UNDO.

   DEF BUFFER bServiceRequest FOR MSRequest.   
   DEF BUFFER bMobSub FOR MobSub.

   /* ongoing ACC */
   IF CAN-FIND
      (FIRST MsRequest WHERE
             MsRequest.MsSeq = MobSub.MsSeq AND
             MsRequest.ReqType = {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} AND 
       LOOKUP(STRING(MsRequest.ReqStat),{&REQ_INACTIVE_STATUSES}) = 0) THEN      
      RETURN NO.
   
   /* ongoing STC */
   IF CAN-FIND
      (FIRST MsRequest WHERE
             MsRequest.MsSeq = MobSub.MsSeq AND
             MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
       LOOKUP(STRING(MsRequest.ReqStat),{&REQ_INACTIVE_STATUSES}) = 0) THEN      
      RETURN NO.
   
   /* ongoing renove contract request */
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = MobSub.MsSeq AND
            MsRequest.ReqType = {&REQTYPE_AFTER_SALES_ORDER},
      EACH bServiceRequest NO-LOCK WHERE
           bServiceRequest.OrigRequest = MsRequest.MsRequest AND
           LOOKUP(STRING(bServiceRequest.ReqStat),{&REQ_INACTIVE_STATUSES}) = 0:
      
      RETURN NO.
   END.

   FIND FIRST Order NO-LOCK WHERE 
              Order.MsSeq = Mobsub.MsSeq AND
              Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} AND
              Order.OrderType < 2 NO-ERROR.
   IF NOT AVAIL Order THEN   
      RETURN NO.
   
   /* Check barrings */
   IF Mobsub.PayType EQ FALSE THEN DO:
      FOR EACH bMobsub NO-LOCK WHERE
               bMobsub.Brand = Syst.Var:gcBrand AND
               bMobsub.AgrCust = Customer.AgrCust AND
               bMobsub.PayType = FALSE AND
               bMobsub.MsStatus = {&MSSTATUS_BARRED}:
         lcBarrStatus = fCheckStatus(bMobsub.MsSeq).
         llBarrings = (LOOKUP(lcBarrStatus, {&FRAUD_BARR_CODES}) > 0 ).      
         IF llBarrings THEN       
            RETURN NO.
      END.   
   END.         
   
   IF CAN-FIND( FIRST Invoice NO-LOCK WHERE
                      Invoice.Brand    = Syst.Var:gcBrand            AND
                      Invoice.Custnum  = MobSub.Custnum     AND            
                      Invoice.InvDate >= ldtFirstDay        AND
                      Invoice.InvType  = {&INV_TYPE_NORMAL} AND             
                      Invoice.DueDate  < TODAY - 2          AND
                      Invoice.PaymState NE 2                AND
                      Invoice.InvAmt     > 0) THEN
            
      RETURN NO.         

   IF fOngoingOrders(MobSub.Cli,"renewal") THEN
      RETURN NO.

   /* get the latest purchased terminal (phone) */
   FIND FIRST SubsTerminal WHERE
              SubsTerminal.MsSeq = MobSub.MsSeq AND
              SubsTerminal.TerminalType = ({&TERMINAL_TYPE_PHONE})
              NO-LOCK USE-INDEX MsSeq NO-ERROR.

   IF AVAIL SubsTerminal THEN 
   DO:
      IF Mobsub.PayType AND
         SubsTerminal.OrderID > 0 AND
         CAN-FIND(FIRST Order NO-LOCK WHERE
                        Order.Brand = Syst.Var:gcBrand AND
                        Order.OrderID = SubsTerminal.OrderId AND
                        Order.OrderType = 2) THEN 
      DO:
         llPrerenove = TRUE.    
         /* Check cancelled same renewal order */
         IF CAN-FIND(FIRST MsRequest WHERE
                           MsRequest.MsSeq = Mobsub.MsSeq AND
                           MsRequest.ReqType = {&REQTYPE_REVERT_RENEWAL_ORDER} AND
                           MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
                           MsRequest.ReqIParam1 = SubsTerminal.OrderID NO-LOCK)
         THEN llCancelledPrerenove = TRUE.
      END. /* IF Mobsub.PayType = True AND */   
   
      Func.Common:mSplitTS(SubsTerminal.PurchaseTS, OUTPUT ldaLastTerminal, OUTPUT liTime).
   
      IF SubsTerminal.PerContractID > 0 THEN 
      DO:
         FIND FIRST DCCLI WHERE
                    DCCLI.MsSeq = Mobsub.MsSeq AND
                    DCCLI.PerContractID = SubsTerminal.PerContractID
         NO-LOCK NO-ERROR.
         IF NOT AVAIL DCCLI OR DCCLI.ValidTo < TODAY THEN ldaLastTerminal = ?.
      END.
   END.
   /* New renewal rules  */   
   IF ldaLastTerminal NE ? THEN 
   DO:
      liConfigDays = 180.
      IF MobSub.PayType THEN 
      DO:
         /* If renewal order is cancelled then it should be allowed */
         IF llPrerenove AND llCancelledPrerenove THEN .
         ELSE IF (TODAY - ldaLastTerminal) < liConfigDays THEN 
            RETURN NO.                              
      END.
      ELSE 
      DO:
        IF (TODAY - ldaLastTerminal) < liConfigDays THEN
         RETURN NO.
      END.   
   END. /* IF MobSub.PayType THEN DO: */
   /* SIM Only */
   ELSE 
   DO:
      IF MobSub.PayType THEN liConfigDays = 180.
      ELSE liConfigDays = 90.

   /* Validate prepaid/postpaid SIM Only config date */
      IF (TODAY - MobSub.ActivationDate) < liConfigDays THEN 
         RETURN NO.         
   END. /* ELSE DO: */

   RETURN YES.

END FUNCTION.

FUNCTION fPersonIdCheck RETURNS LOGICAL (INPUT liPersonIdType AS INT):

   DEFINE VAR liPersonId AS LOGICAL NO-UNDO INIT YES.
   IF liPersonIdType = 1 AND 
      NOT CAN-FIND(FIRST Customer WHERE
                         Customer.Brand   = Syst.Var:gcBrand AND
                         Customer.CustNum = MobSub.CustNum AND
                         Customer.CustIdType = "NIF") THEN
      liPersonId = NO.
   ELSE IF liPersonIdType = 2 AND
        NOT CAN-FIND(FIRST Customer WHERE
                           Customer.Brand   = Syst.Var:gcBrand AND
                           Customer.CustNum = MobSub.CustNum AND
                           Customer.CustIdType = "NIE") THEN
      liPersonId = NO.
   ELSE IF liPersonIdType = 3 AND
        NOT CAN-FIND(FIRST Customer WHERE
                           Customer.Brand   = Syst.Var:gcBrand AND
                           Customer.CustNum = MobSub.CustNum AND
                           Customer.CustIdType = "CIF") THEN
      liPersonId = NO.
   ELSE IF liPersonIdType = 4 AND
        NOT CAN-FIND(FIRST Customer WHERE
                           Customer.Brand   = Syst.Var:gcBrand AND
                           Customer.CustNum = MobSub.CustNum AND
                           Customer.CustIdType = "PASSPORT") THEN
      liPersonId = NO.

   RETURN liPersonId.
END FUNCTION.

EACH_MOBSUB:
FOR EACH MobSub NO-LOCK WHERE
         MobSub.Brand   = Syst.Var:gcBrand AND 
         (IF pcCliType NE "" THEN MobSub.CLIType = pcCliType ELSE TRUE):

   /* If MobSub loop executes more than 30 seconds 
   then it should terminate the execution */
   IF (liLoopCount MOD 100) = 0 THEN DO:
      liLoopEndTime = TIME.

      IF liLoopEndTime - liLoopBegTime >= 30 THEN LEAVE EACH_MOBSUB.

   END.

   liLoopCount = liLoopCount + 1. 
   
      /* Data bundle  */  
   IF pcDataBundleId > "" THEN
   DO:   
      lcDataBundles = fGetCurrentSpecificBundle(MobSub.MsSeq,"BONO").      
      IF pcDataBundleId = "None" THEN       
      DO:  
         IF lcDataBundles > "" THEN 
            NEXT EACH_MOBSUB.         
      END.
      ELSE IF pcDataBundleId <> lcDataBundles THEN 
            NEXT EACH_MOBSUB.                  
   END. /* pcDataBundleId > "" */
   
   IF plDebt AND NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                                    Invoice.Brand    = Syst.Var:gcBrand            AND
                                    Invoice.Custnum  = MobSub.Custnum     AND            
                                    Invoice.InvDate >= ldtFirstDay        AND
                                    Invoice.InvType  = {&INV_TYPE_NORMAL} AND             
                                    Invoice.DueDate  < TODAY - 2          AND
                                    Invoice.PaymState NE 2                AND
                                    Invoice.InvAmt     > 0) THEN
            
      NEXT EACH_MOBSUB.  

      /* Segmentation offer */
   IF pcSegmentOffer > "" AND
      NOT CAN-FIND (FIRST Segmentation NO-LOCK WHERE
                          Segmentation.MsSeq = MobSub.MsSeq AND
                          pcSegmentOffer = Segmentation.SegmentOffer) THEN
      NEXT EACH_MOBSUB.    
   
      /* PAYTERMX */
   IF pcPayTerm > "" THEN DO:
      IF pcPayTerm = "Q25" THEN DO:
         IF NOT CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                               DCCLI.MsSeq    = MobSub.MsSeq AND
                               DCCLI.DCEvent  BEGINS "RVTERM" AND
                               DCCLI.ValidTo >= TODAY) THEN NEXT EACH_MOBSUB.
      END.
      ELSE IF LOOKUP(pcPayTerm,"Q25") > 0 THEN DO:
         IF NOT CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                               DCCLI.MsSeq    = MobSub.MsSeq AND
                              (DCCLI.DCEvent  = pcPayTerm OR
                               DCCLI.DCEvent  BEGINS "RVTERM") AND
                               DCCLI.ValidTo >= TODAY) THEN NEXT EACH_MOBSUB.
      END.
      ELSE IF pcPayTerm = "existing" THEN DO:
         IF NOT CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                               DCCLI.MsSeq    = MobSub.MsSeq AND
                               DCCLI.DCEvent  BEGINS "PAYTERM" AND
                               DCCLI.ValidTo >= TODAY) THEN NEXT EACH_MOBSUB. 
      END.
      ELSE IF pcPayTerm = "none" THEN DO:
         IF CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                           DCCLI.MsSeq    = MobSub.MsSeq AND
                           DCCLI.DCEvent  BEGINS "PAYTERM" AND
                           DCCLI.ValidTo >= TODAY) THEN NEXT EACH_MOBSUB.
      END.
      ELSE DO:
         IF NOT CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                               DCCLI.MsSeq    = MobSub.MsSeq AND
                               DCCLI.DCEvent  = pcPayTerm AND
                               DCCLI.ValidTo >= TODAY) THEN NEXT EACH_MOBSUB.
      END.
   END.
 
   /* TERMX */
   liCount = 0. 
   /* YDA-895 licount reset was needed to prevent unwanted numbers 
      to be added for result of RPC*/
   IF pcTerm > "" THEN DO:
      IF pcTerm EQ "none" THEN DO:
         FOR EACH DCCLI NO-LOCK WHERE
                  DCCLI.MsSeq      = MobSub.MsSeq AND
                  DCCLI.DCEvent   BEGINS "TERM"   AND
                  DCCLI.ValidFrom <= TODAY        AND
                  DCCLI.ValidTo   >= TODAY        AND
                  DCCLI.CreateFees = TRUE         BY DCCLI.ValidFrom DESC:
 
            IF CAN-FIND(FIRST DayCampaign NO-LOCK WHERE
                              DayCampaign.Brand        = Syst.Var:gcBrand            AND
                              DayCampaign.DCEvent      = DCCLI.DCEvent      AND
                              DayCampaign.DCType       = {&DCTYPE_DISCOUNT} AND
                              DayCampaign.TermFeeModel NE ""                AND
                              DayCampaign.TermFeeCalc > 0) THEN
            liCount = liCount + 1.
 
            IF liCount > 0 THEN NEXT EACH_MOBSUB.
         END. /* FOR EACH DCCLI NO-LOCK WHERE */
      END.
      ELSE IF pcTerm EQ "existing" THEN DO:
         FOR EACH DCCLI NO-LOCK WHERE
                  DCCLI.MsSeq      = MobSub.MsSeq AND
                  DCCLI.DCEvent   BEGINS "TERM"   AND
                  DCCLI.ValidFrom <= TODAY        AND
                  DCCLI.ValidTo   >= TODAY        AND
                  DCCLI.CreateFees = TRUE         BY DCCLI.ValidFrom DESC:
 
            IF CAN-FIND(FIRST DayCampaign NO-LOCK WHERE
                              DayCampaign.Brand        = Syst.Var:gcBrand            AND
                              DayCampaign.DCEvent      = DCCLI.DCEvent      AND
                              DayCampaign.DCType       = {&DCTYPE_DISCOUNT} AND
                              DayCampaign.TermFeeModel NE ""                AND
                              DayCampaign.TermFeeCalc > 0) THEN
            liCount = liCount + 1.
            IF liCount > 0 THEN LEAVE.
         END. /* FOR EACH DCCLI NO-LOCK WHERE */
         IF liCount = 0 THEN NEXT EACH_MOBSUB.
      END.
      ELSE DO:
         FOR EACH DCCLI NO-LOCK WHERE
                  DCCLI.MsSeq      = MobSub.MsSeq AND
                  DCCLI.ValidFrom <= TODAY        AND
                  DCCLI.ValidTo   >= TODAY        AND
                  DCCLI.CreateFees = TRUE         BY DCCLI.ValidFrom DESC:
                  
            IF CAN-FIND(FIRST DayCampaign NO-LOCK WHERE
                              DayCampaign.Brand        = Syst.Var:gcBrand            AND
                              DayCampaign.DCEvent      = DCCLI.DCEvent      AND
                              DayCampaign.DCEvent      = pcterm             AND
                              DayCampaign.DCType       = {&DCTYPE_DISCOUNT} AND
                              DayCampaign.TermFeeModel NE ""                AND
                              DayCampaign.TermFeeCalc > 0) THEN  
            liCount = liCount + 1.
            
            IF liCount > 0 THEN LEAVE.
         END. /* FOR EACH DCCLI NO-LOCK WHERE */
         IF liCount = 0 THEN NEXT EACH_MOBSUB.
      END.
   END. 

   IF pcLanguage > "" AND
      NOT CAN-FIND(FIRST OrderCustomer WHERE
                         OrderCustomer.custNum EQ Mobsub.custnum AND
                         OrderCustomer.Language EQ pcLanguage NO-LOCK) THEN
         NEXT EACH_MOBSUB.   
 
   IF pcInvGroup > "" AND
      NOT CAN-FIND(FIRST Customer WHERE 
                         Customer.brand EQ Syst.Var:gcBrand AND
                         Customer.custnum EQ MobSub.CustNum AND
                         Customer.invGroup EQ pcInvGroup NO-LOCK) THEN
      NEXT EACH_MOBSUB.

   /* Postpaid or Prepaid */
   IF plPayType <> MobSub.PayType THEN NEXT EACH_MOBSUB.

   /* YDA-895 Voice or Data  */
   IF NOT CAN-FIND(FIRST CliType WHERE 
                         CliType.Brand     = Syst.Var:gcBrand AND
                         CliType.CliType   = MobSub.CliType AND
                         CliType.UsageType = piUsageType NO-LOCK) THEN
      NEXT EACH_MOBSUB.
   /* YDA-895 ID of the customer */   
   IF piPersonIdType > 0 AND NOT fPersonIdCheck(INPUT piPersonIdType) THEN
      NEXT EACH_MOBSUB.   

   IF pdtEndDate <> ? AND 
      MobSub.TariffActDate >= pdtEndDate THEN NEXT EACH_MOBSUB.

   /* YDA-895 To narrow down the list */
   IF pdtStartDate <> ? AND 
      MobSub.TariffActDate < pdtStartDate THEN NEXT EACH_MOBSUB.

   IF pdtEndDate <> ? OR pdtStartDate <> ? THEN DO: 
      FOR EACH Order NO-LOCK WHERE 
               Order.MsSeq  EQ MobSub.MsSeq AND
               LOOKUP(Order.StatusCode, {&ORDER_CLOSE_STATUSES}) = 0
           BY Order.CrStamp DESC:
       
          Func.Common:mSplitTS(Order.CrStamp, 
                   OUTPUT ldtOrderDate,
                   OUTPUT liOrderTime).
          
          IF pdtEndDate <> ? AND ldtOrderDate >= pdtEndDate THEN 
             NEXT EACH_MOBSUB.
          ELSE IF pdtStartDate <> ? AND ldtOrderDate < pdtStartDate THEN
             NEXT EACH_MOBSUB. /* YDA-895 to narrow down the number of results */
      END.          
   END.

   IF plgOrderStatus THEN DO: 
      FOR EACH Order NO-LOCK WHERE 
               Order.MsSeq  EQ MobSub.MsSeq
           BY Order.CrStamp DESC:
       
         IF NOT Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} THEN
             NEXT EACH_MOBSUB. 
      END.          
   END.

   IF pcOrderType > "" THEN DO:
      CASE pcOrderType:
         WHEN "sim" THEN DO:
           IF CAN-FIND(FIRST SubsTerminal NO-LOCK WHERE
                             SubsTerminal.Brand = Syst.Var:gcBrand AND
                             SubsTerminal.MsSeq = MobSub.MsSeq AND
                             SubsTerminal.TerminalType = {&TERMINAL_TYPE_PHONE})
                             THEN NEXT EACH_MOBSUB.
         END.
         WHEN "terminal" THEN DO:
           IF NOT CAN-FIND(FIRST SubsTerminal NO-LOCK WHERE
                                 SubsTerminal.Brand = Syst.Var:gcBrand AND
                                 SubsTerminal.MsSeq = MobSub.MsSeq AND
                                 SubsTerminal.TerminalType = {&TERMINAL_TYPE_PHONE})
                                 THEN NEXT EACH_MOBSUB.
         END.
         OTHERWISE NEXT EACH_MOBSUB.
      END CASE.
   END.

   /* Subscription type */
   IF LOOKUP(pcCliType,lcBundleCLITypes) > 0 AND
      pcSubsBundleId <> MobSub.TariffBundle THEN NEXT EACH_MOBSUB.   
   
   /* Eligible for renewal order YDA-895 */
   IF plgEligibleRenewal <> ? THEN 
   DO:
      IF plgEligibleRenewal <> fCheckEligibleRenewal() THEN
         NEXT EACH_MOBSUB.      
   END.      
 
 /*Other bundles  */
   DO liCount = 1 TO liNumberOfBundles:
      lcOtherBundle = ENTRY(liCount,pcOtherBundles).
      CASE lcOtherBundle:
         WHEN "DSS" THEN
            IF NOT fIsDSSActive(MobSub.CustNum,Func.Common:mMakeTS()) THEN NEXT EACH_MOBSUB.
         OTHERWISE NEXT.
      END. /* CASE lcOtherBundle: */
   END. /* DO liCount = 1 TO liNumberOfBundles: */
   
   /*Any Barrings Exist*/
   IF plBarring AND Func.BarrMethod:mGetActiveBarrings(MobSub.MsSeq) EQ "" THEN 
      NEXT EACH_MOBSUB.

   /* YDA-1012 and YDA-1017  */
   IF CAN-FIND(FIRST Order WHERE Order.MsSeq = MobSub.MsSeq AND
                                 Order.OrderChannel = "VIP") THEN
      NEXT EACH_MOBSUB.

   FOR EACH bfMobSub WHERE bfMobSub.Brand   = MobSub.Brand AND
                           bfMobsub.CustNum = MobSub.CustNum NO-LOCK:
      IF CAN-FIND(FIRST CLIType WHERE CLIType.Brand   = bfMobSub.Brand AND 
                                      CLIType.CliType = bfMobSub.CliTYpe AND
                                      CLIType.WebStatusCode = 0) THEN
         NEXT EACH_MOBSUB.                              
                           
   END.                        
 
      /* Count number of subscriptions */
   IF liNumberOfSubs <= piOffset AND piOffset > 0 THEN DO:
      liNumberOfSubs = liNumberOfSubs + 1.
      NEXT EACH_MOBSUB.
   END.
   ELSE DO:
      resp_struct = add_struct(resp_array, "").
      add_string(resp_struct, "cli", MobSub.CLI).
      add_timestamp(resp_struct, "act_stamp", MobSub.ActivationTS).
      liNumberOfSubs = liNumberOfSubs + 1.
      liNumberLimit  = liNumberLimit  + 1.
   END.   
 
   IF liNumberLimit >= piSubsLimit THEN LEAVE EACH_MOBSUB.
END.

FINALLY:
   END.


