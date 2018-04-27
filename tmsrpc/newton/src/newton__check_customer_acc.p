/**
 * Check if customer may do an order.
 *
 * @input   brand;string;mandatory
            person_id;string;mandatory;
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
                   extra_line_allowed;string;mandatory;comma separated list of allowed extra lines
                   segment;string;mandatory;
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/orderchk.i}
{Func/custfunc.i}
{Func/profunc.i}
{Func/barrfunc.i}

/* Input parameters */
DEF VAR pcTenant         AS CHAR NO-UNDO.
DEF VAR pcPersonId       AS CHAR NO-UNDO.
DEF VAR pcIdType         AS CHAR NO-UNDO.
DEF VAR plSelfEmployed   AS LOG  NO-UNDO.
DEF VAR piOrders         AS INT  NO-UNDO.
DEF VAR pcCliType        AS CHAR NO-UNDO.
DEF VAR pcChannel        AS CHAR NO-UNDO.
DEF VAR top_array        AS CHAR NO-UNDO.
DEF VAR plSTCMigrate     AS LOG  NO-UNDO.

/* Local variable */
DEF VAR llOrderAllowed                AS LOG  NO-UNDO.
DEF VAR lcReason                      AS CHAR NO-UNDO.
DEF VAR lcReturnStruct                AS CHAR NO-UNDO.
DEF VAR liSubLimit                    AS INT  NO-UNDO.
DEF VAR lisubs                        AS INT  NO-UNDO.
DEF VAR lcAddLineAllowed              AS CHAR NO-UNDO. 
DEF VAR liActLimit                    AS INT  NO-UNDO.
DEF VAR liacts                        AS INT  NO-UNDO.
DEF VAR lcSegment                     AS CHAR NO-UNDO.
DEF VAR llProChannel                  AS LOG  NO-UNDO.
DEF VAR llCustCatPro                  AS LOG  NO-UNDO.
DEF VAR lcPROChannels                 AS CHAR NO-UNDO.
DEF VAR lcCategory                    AS CHAR NO-UNDO.
DEF VAR llPROOngoingOrder             AS LOGI NO-UNDO.
DEF VAR llNonProOngoingOrder          AS LOGI NO-UNDO.
DEF VAR liMobsubCount                 AS LOGI NO-UNDO.
DEF VAR lcExtraLineAllowed            AS CHAR NO-UNDO. 
DEF VAR llNonProToProMigrationOngoing AS LOGI NO-UNDO.
DEF VAR llProToNonProMigrationOngoing AS LOGI NO-UNDO.
DEF VAR lcResult                      AS CHAR NO-UNDO.
DEF VAR liMLMsSeq                     AS INT NO-UNDO. 
DEFINE VARIABLE lii AS INTEGER NO-UNDO.
DEFINE VARIABLE lcExtraLineCLITypes AS CHARACTER NO-UNDO.

top_array = validate_request(param_toplevel_id, "string,string,string,boolean,int,[string],[string],[boolean]").
IF top_array EQ ? THEN RETURN.

ASSIGN
   pcTenant       = get_string(param_toplevel_id, "0")
   pcPersonId     = get_string(param_toplevel_id, "1")
   pcIdType       = get_string(param_toplevel_id, "2")
   plSelfEmployed = get_bool(param_toplevel_id, "3")
   piOrders       = get_int(param_toplevel_id, "4").

IF NUM-ENTRIES(top_array) EQ 6 THEN
   pcCliType   = get_string(param_toplevel_id, "5").
ELSE IF NUM-ENTRIES(top_array) EQ 7 THEN 
   ASSIGN
      pcCliType   = get_string(param_toplevel_id, "5")
      pcChannel   = get_string(param_toplevel_id, "6").

ELSE IF NUM-ENTRIES(top_array) GT 7 THEN
   ASSIGN
      pcCliType    = get_string(param_toplevel_id, "5")
      pcChannel    = get_string(param_toplevel_id, "6")
      plSTCMigrate = get_bool(param_toplevel_id, "7").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

IF INDEX(pcChannel,"PRO") > 0 THEN 
    llProChannel = TRUE.

llOrderAllowed = Func.ValidateACC:mSubscriptionLimitCheck(
   pcPersonId,
   pcIdType,
   plSelfEmployed,
   llProChannel,
   piOrders,
   OUTPUT liSubLimit,
   OUTPUT lisubs,
   OUTPUT liActLimit,
   OUTPUT liActs).
   
/* do not raise error with STC orders */
IF NOT llOrderAllowed THEN DO:
   IF plSTCMigrate THEN
      llOrderAllowed = TRUE.
   ELSE lcReason = "subscription limit".
END.

FIND FIRST Customer NO-LOCK WHERE
           Customer.Brand      = Syst.Var:gcBrand    AND
           Customer.OrgID      = pcPersonId AND
           Customer.CustIDType = pcIdType   AND
           Customer.Roles     NE "inactive" NO-ERROR.
       
/* check barring subscriptions */
IF AVAIL Customer AND
   fExistBarredSubForCustomer(Customer.CustNum) THEN ASSIGN
   lcReason = "barring"
   llOrderAllowed = FALSE.

ASSIGN
    lcPROChannels    = fCParamC("PRO_CHANNELS").

lcSegment    = fgetCustSegment(pcIdType, 
                               plSelfEmployed, 
                               llProChannel,
                               pcPersonId,  /* YDR-2621 */
                               OUTPUT lccategory).

/* If customer does not have subscriptions it is handled as new */

IF AVAIL Customer AND
   NOT llProChannel AND
   NOT plSelfEmployed AND
   (fGetSegment(Customer.custnum, 0) EQ "AUTONOMO" OR
    fGetSegment(Customer.custnum, 0) EQ "SOHO-AUTONOMO") THEN DO:
   ASSIGN
      llOrderAllowed = FALSE
      lcReason = "The customer must have self employed set to true".
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

lcExtraLineCLITypes = fExtraLineCLITypes().

DO lii = 1 TO NUM-ENTRIES(lcExtraLineCLITypes):
   IF fCheckExistingMainLineAvailForExtraLine(ENTRY(lii,lcExtraLineCLITypes), 
                                              pcIdType,
                                              pcPersonId, 
                                              OUTPUT liMLMsSeq) > 0                               OR
      fCheckOngoingMainLineAvailForExtraLine(ENTRY(lii,lcExtraLineCLITypes), pcIdType,pcPersonId) > 0
   THEN lcExtraLineAllowed = lcExtraLineAllowed + "," + ENTRY(lii,lcExtraLineCLITypes).
END.

lcExtraLineAllowed = LEFT-TRIM(lcExtraLineAllowed, ",").

IF lcAddLineAllowed = "" THEN DO:
      
   FOR EACH OrderCustomer NO-LOCK WHERE   
            OrderCustomer.Brand      EQ Syst.Var:gcBrand AND 
            OrderCustomer.CustId     EQ pcPersonId AND
            OrderCustomer.CustIdType EQ pcIdType AND
            OrderCustomer.RowType    EQ 1,
      EACH  Order NO-LOCK WHERE
            Order.Brand              EQ Syst.Var:gcBrand AND
            Order.orderid            EQ OrderCustomer.Orderid AND
            Order.OrderType          NE {&ORDER_TYPE_RENEWAL} AND 
            Order.OrderType          NE {&ORDER_TYPE_STC} AND 
            Order.OrderType          NE {&ORDER_TYPE_ACC} AND 
            Order.SalesMan NE "GIFT" AND
            LOOKUP(STRING(Order.statuscode),{&ORDER_INACTIVE_STATUSES}) EQ 0,
       FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = Syst.Var:gcBrand AND
             CLIType.CLIType = Order.CLIType AND
             CLIType.LineType > 0,
       EACH OrderAction NO-LOCK WHERE 
            OrderAction.Brand = Order.Brand AND
            OrderAction.OrderId = Order.OrderID AND
            OrderAction.ItemType = "BundleItem":

         IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                           CLIType.Brand    = Syst.Var:gcBrand                   AND
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

IF lcExtraLineAllowed EQ ""
THEN lcExtraLineAllowed = "NO_MAIN_LINE".

add_string(lcReturnStruct, 'extra_line_allowed',lcExtraLineAllowed).

IF liSubs >= liSubLimit AND NOT plSTCMigrate THEN
   add_boolean(lcReturnStruct,"subscription_limit_reached",TRUE).
ELSE
   add_boolean(lcReturnStruct,"subscription_limit_reached",FALSE).
IF liActs >= liActLimit AND NOT plSTCMigrate THEN
   add_boolean(lcReturnStruct,"activation_limit_reached",TRUE).
ELSE
   add_boolean(lcReturnStruct,"activation_limit_reached",FALSE).

add_string(lcReturnStruct, 'segment',lcSegment).
