/**
 * Check if customer may do an order.
 *
 * @input   person_id;string;mandatory;
            id_type;string;mandatory;
            self_employed;bool;mandatory;
            orders;int;mandatory;
            clitype;string;optional;
            order_channel;string;optional;
 *          
 * @output   check_customer;struct;mandatory; response structure
 * @check_customer order_allowed;boolean;mandatory;
                   subscription_limit;int;mandatory;
                   reason;string;optional;possible fail reason, returned if order_allowed = false
                   additional_line_allowed;string;mandatory;OK,NO_MAIN_LINE,NO_SUBSCRIPTIONS (OK is returned also if there's no active main line but a pending main line order)
                   segment;string;mandatory;
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
{Func/orderchk.i}
{Func/custfunc.i}
{Syst/tmsconst.i}
gcBrand = "1".

/* Input parameters */
DEF VAR pcPersonId       AS CHAR NO-UNDO.
DEF VAR pcIdType         AS CHAR NO-UNDO.
DEF VAR plSelfEmployed   AS LOG  NO-UNDO.
DEF VAR piOrders         AS INT  NO-UNDO.
DEF VAR pcCliType        AS CHAR NO-UNDO.
DEF VAR pcChannel        AS CHAR NO-UNDO.
DEF VAR top_array        AS CHAR NO-UNDO.
DEF VAR plSTCMigrate     AS LOG  NO-UNDO.

/* Local variable */
DEF VAR llOrderAllowed             AS LOG  NO-UNDO.
DEF VAR lcReason                   AS CHAR NO-UNDO.
DEF VAR lcReturnStruct             AS CHAR NO-UNDO.
DEF VAR liSubLimit                 AS INT  NO-UNDO.
DEF VAR lisubs                     AS INT  NO-UNDO.
DEF VAR lcAddLineAllowed           AS CHAR NO-UNDO. 
DEF VAR liActLimit                 AS INT  NO-UNDO.
DEF VAR liacts                     AS INT  NO-UNDO.
DEF VAR lcSegment                  AS CHAR NO-UNDO.
DEF VAR llProChannel               AS LOG  NO-UNDO.
DEF VAR llCustCatPro               AS LOG  NO-UNDO.
DEF VAR lcPROChannels              AS CHAR NO-UNDO.
DEF VAR lcnonPROChannels           AS CHAR NO-UNDO.
DEF VAR lcCategory                 AS CHAR NO-UNDO.
DEF VAR llPROOngoingOrder          AS LOGI NO-UNDO.
DEF VAR llNonProOngoingOrder       AS LOGI NO-UNDO.
DEF VAR liMobsubCount              AS LOGI NO-UNDO.
DEF VAR llNonToProMigrationOngoing AS LOGI NO-UNDO.

top_array = validate_request(param_toplevel_id, "string,string,boolean,int,[string],[string],[boolean]").
IF top_array EQ ? THEN RETURN.

ASSIGN
   pcPersonId     = get_string(param_toplevel_id, "0")
   pcIdType       = get_string(param_toplevel_id, "1")
   plSelfEmployed = get_bool(param_toplevel_id, "2")
   piOrders       = get_int(param_toplevel_id, "3").

IF NUM-ENTRIES(top_array) EQ 5 THEN
   pcCliType   = get_string(param_toplevel_id, "4").
ELSE IF NUM-ENTRIES(top_array) EQ 6 THEN 
   ASSIGN
      pcCliType   = get_string(param_toplevel_id, "4")
      pcChannel   = get_string(param_toplevel_id, "5").

ELSE IF NUM-ENTRIES(top_array) GT 6 THEN
   ASSIGN
      pcCliType    = get_string(param_toplevel_id, "4")
      pcChannel    = get_string(param_toplevel_id, "5")
      plSTCMigrate = get_bool(param_toplevel_id, "6").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF INDEX(pcChannel,"PRO") > 0 THEN 
    llProChannel = TRUE.

FIND FIRST Customer NO-LOCK WHERE
           Customer.Brand      = gcBrand    AND
           Customer.OrgID      = pcPersonId AND
           Customer.CustIDType = pcIdType   AND
           Customer.Roles     NE "inactive" NO-ERROR.
IF AVAIL Customer THEN 
DO:
   FIND FIRST CustCat WHERE Custcat.brand EQ "1" AND CustCat.category EQ Customer.category NO-LOCK NO-ERROR.
   IF AVAIL CustCat THEN
      ASSIGN 
          llCustCatPro = CustCat.pro
          lcSegment    = CustCat.Segment. 
END.
ELSE
   lcSegment = fgetCustSegment(pcIdType, plSelfEmployed, llProChannel, OUTPUT lccategory).

llOrderAllowed = fSubscriptionLimitCheck(
   pcPersonId,
   pcIdType,
   plSelfEmployed,
   llProChannel,
   piOrders,
   OUTPUT lcReason, 
   OUTPUT liSubLimit,
   OUTPUT lisubs,
   OUTPUT liActLimit,
   OUTPUT liActs).

lcPROChannels = fCParamC("PRO_CHANNELS").
lcnonPROChannels = fCParamC("NON_PRO_CHANNELS").

ASSIGN llNonToProMigrationOngoing = fCheckOngoingProMigration(Customer.CustNum).

IF LOOKUP(pcChannel,lcPROChannels) > 0 THEN 
DO:
   IF AVAIL Customer THEN 
   DO:
      IF llCustCatPro THEN 
      DO:
          IF NOT (fCheckExistingConvergentWithoutALCheck (pcIdType, pcPersonId, pcCliType) OR 
                  fCheckOngoingConvergentOrderWithoutALCheck(pcIdType, pcPersonId, pcCliType)) THEN
          DO:
              llOrderAllowed = FALSE.
              lcReason = "Additional mobile line is not compatible with respective to main convergent line".
          END.   
          ELSE 
          DO:
             ORDCUSTOMERLOOP:
             FOR EACH OrderCustomer WHERE OrderCustomer.CustNum = Customer.CustNum                   AND
                                          OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} AND 
                                          OrderCustomer.Pro     = FALSE                              NO-LOCK:

                 FIND FIRST Order WHERE Order.Brand EQ gcBrand AND Order.OrderId EQ OrderCustomer.OrderId NO-LOCK NO-ERROR.
                 IF AVAIL Order AND LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 THEN 
                 DO:
                     llOrderAllowed = FALSE.
                     lcReason = "ongoing non PRO order".
                     LEAVE ORDCUSTOMERLOOP.    
                 END.

             END.
          END.
      END.
      ELSE 
      DO: /* NOT llCustCatPro */
          IF plSTCMigrate THEN 
          DO:
              IF LOOKUP(pcIdType,"NIF,NIE") > 0 AND NOT plSelfEmployed THEN
                  ASSIGN 
                      llOrderAllowed = FALSE
                      lcReason = "PRO migration not possible because of multible mobile lines".
              ELSE 
              DO:
                  FIND Mobsub WHERE Mobsub.Brand EQ gcBrand AND Mobsub.InvCust EQ Customer.CustNum NO-LOCK NO-ERROR.
                  IF AMBIG MobSub THEN 
                      ASSIGN 
                          llOrderAllowed = FALSE
                          lcReason = "PRO migration not possible because of multible mobile lines".
                  ELSE IF AVAIL MobSub THEN 
                  DO:
                      IF fIsConvergenceTariff(Mobsub.Clitype) THEN
                          ASSIGN
                              llOrderAllowed = FALSE
                              lcReason = "PRO migration not possible for convergent".
                      ELSE 
                      DO:        
                          FIND FIRST CliType WHERE CliType.Brand = gcBrand AND CliType.CliType = pcCliType NO-LOCK NO-ERROR.
                          IF AVAIL CliType AND CliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
                              ASSIGN 
                                  llOrderAllowed = FALSE
                                  lcReason = "Mobile line for non-pro customer from PRO channel".
                      END.
                  END.
                  ELSE IF NOT llNonToProMigrationOngoing THEN 
                      ASSIGN 
                          llOrderAllowed = FALSE
                          lcReason = "PRO migration not possible because, no mobile lines exists".        
              END.
          END.
          ELSE DO:
              /* PRO Order Allowed for non pro customer if customer do not have active subscriptions or ongoing ordres */
              /*IF ftegoryChangeAllowed(Customer.CustNum) EQ TRUE THEN  /*YTS-11228/1 - function comes from branch of YTS-11227, fix this in merge*/
                 llOrderAllowed = TRUE.*/
              ASSIGN
                  llOrderAllowed = FALSE
                  lcReason       = "non PRO customer".
         END.
      END.
   END.
END.
ELSE IF LOOKUP(pcChannel,lcnonPROChannels) > 0 THEN 
DO:
   IF AVAIL Customer THEN 
   DO:
      IF llCustCatPro OR llNonToProMigrationOngoing THEN 
      DO:
          llOrderAllowed = FALSE.
          lcReason = "customer already exists with PRO category".
      END.
   END.
END.

/* Removed legacy main-additional line code, as it is not 
   required any more to support it */ 

IF LOOKUP(pcCliType,{&ADDLINE_CLITYPES}) > 0 THEN DO:
   IF fCheckExistingConvergent(pcIdType,pcPersonId,pcCliType) THEN 
      lcAddLineAllowed = "OK".
   ELSE IF fCheckOngoingConvergentOrder(pcIdType,pcPersonId,pcCliType) THEN 
      lcAddLineAllowed = "OK".
   ELSE IF fCheckExistingMobileOnly(pcIdType,pcPersonId,pcCliType) THEN 
      lcAddLineAllowed = "MOBILE_ONLY". /* Additional Line with mobile only ALFMO-5 */
   ELSE IF fCheckOngoingMobileOnly(pcIdType,pcPersonId,pcCliType) THEN 
      lcAddLineAllowed = "MOBILE_ONLY". /* Additional Line with mobile only ALFMO-5 */
   ELSE lcAddLineAllowed = "NO_MAIN_LINE".
END.

IF lcAddLineAllowed = "" THEN DO:
      
   FOR EACH OrderCustomer NO-LOCK WHERE   
            OrderCustomer.Brand      EQ gcBrand AND 
            OrderCustomer.CustId     EQ pcPersonId AND
            OrderCustomer.CustIdType EQ pcIdType AND
            OrderCustomer.RowType    EQ 1,
      EACH  Order NO-LOCK WHERE
            Order.Brand              EQ gcBrand AND
            Order.orderid            EQ OrderCustomer.Orderid AND
            Order.OrderType          NE {&ORDER_TYPE_RENEWAL} AND 
            Order.OrderType          NE {&ORDER_TYPE_STC} AND 
            Order.SalesMan NE "GIFT" AND
            LOOKUP(STRING(Order.statuscode),{&ORDER_INACTIVE_STATUSES}) EQ 0,
       FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = gcBrand AND
             CLIType.CLIType = Order.CLIType AND
             CLIType.LineType > 0,
       EACH OrderAction NO-LOCK WHERE 
            OrderAction.Brand = Order.Brand AND
            OrderAction.OrderId = Order.OrderID AND
            OrderAction.ItemType = "BundleItem":

         IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                           CLIType.Brand    = gcBrand                   AND
                           CLIType.CLIType  = OrderAction.ItemKey       AND
                           CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:
         lcAddLineAllowed = "OK".
         LEAVE.
      END.
   END.
END.

IF lcAddLineAllowed EQ "" THEN lcAddLineAllowed = "NO_SUBSCRIPTIONS".

lcReturnStruct = add_struct(response_toplevel_id, "").
add_boolean(lcReturnStruct, 'order_allowed', llOrderAllowed).
add_int(lcReturnStruct, 'subscription_limit', liSubLimit).
IF NOT llOrderAllowed THEN add_string(lcReturnStruct, 'reason',lcReason).
add_string(lcReturnStruct, 'additional_line_allowed',lcAddLineAllowed).

IF liSubs >= liSubLimit THEN
   add_boolean(lcReturnStruct,"subscription_limit_reached",TRUE).
ELSE
   add_boolean(lcReturnStruct,"subscription_limit_reached",FALSE).
IF liActs >= liActLimit THEN
   add_boolean(lcReturnStruct,"activation_limit_reached",TRUE).
ELSE
   add_boolean(lcReturnStruct,"activation_limit_reached",FALSE).

add_string(lcReturnStruct, 'segment',lcSegment).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
