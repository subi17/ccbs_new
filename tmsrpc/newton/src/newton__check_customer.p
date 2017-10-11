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
                   extra_line_allowed;string;mandatory;OK,NO_MAIN_LINE,NO_SUBSCRIPTIONS
                   segment;string;mandatory;
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
{Func/orderchk.i}
{Func/custfunc.i}
{Func/profunc.i}

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
DEF VAR lcExtraLineCLITypes           AS CHAR NO-UNDO.
DEF VAR liMainLineOrderId             AS INT  NO-UNDO. 
DEF VAR liOngoingOrderId              AS INT  NO-UNDO. 
DEF VAR lcExtraLineAllowed            AS CHAR NO-UNDO. 
DEF VAR llNonProToProMigrationOngoing AS LOGI NO-UNDO.
DEF VAR llProToNonProMigrationOngoing AS LOGI NO-UNDO.
DEF VAR lcResult                      AS CHAR NO-UNDO.

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

lcExtraLineCLITypes = fCParam("DiscountType","ExtraLine_CLITypes").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

IF INDEX(pcChannel,"PRO") > 0 THEN 
    llProChannel = TRUE.

FUNCTION fCheckOngoingOrders RETURNS LOGICAL (INPUT icCustId AS CHAR,
                                              INPUT icCustIdType AS CHAR,
                                              INPUT iimsseq AS INT):
   FOR EACH OrderCustomer NO-LOCK WHERE
            OrderCustomer.Brand      EQ gcBrand AND
            OrderCustomer.CustId     EQ icCustId AND
            OrderCustomer.CustIdType EQ icCustIDType AND
            OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
      FIRST Order NO-LOCK WHERE
            Order.Brand              EQ gcBrand AND
            Order.orderid            EQ Ordercustomer.Orderid AND
            Order.msseq NE iimsseq AND
           LOOKUP(Order.StatusCode, {&ORDER_INACTIVE_STATUSES}) = 0:
      RETURN TRUE.
   END.
   RETURN FALSE.
END.

FUNCTION fCheckMigration RETURNS LOG ():

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.

   DEF VAR llOnlyActiveFound AS LOG NO-UNDO.

   IF LOOKUP(pcIdType,"NIF,NIE") > 0 AND NOT plSelfEmployed THEN
      ASSIGN
         llOrderAllowed = FALSE
         lcReason = "PRO migration not possible because of not company or selfemployed".
   ELSE DO:
      FIND Mobsub WHERE Mobsub.Brand EQ gcBrand AND Mobsub.InvCust EQ Customer.CustNum NO-LOCK NO-ERROR.
      IF AMBIG MobSub THEN
          IF (pcChannel EQ "Newton" OR pcChannel EQ "VFR") THEN DO:
             IF CAN-FIND(FIRST Mobsub NO-LOCK WHERE
                               Mobsub.Brand EQ gcBrand AND
                               Mobsub.InvCust EQ Customer.CustNum AND
                               Mobsub.paytype) THEN DO:
               ASSIGN
                  llOrderAllowed = FALSE
                  lcReason = "PRO migration not possible because of prepaid subscription".
             END.
             /* Check any ongoing orders */
             IF fCheckOngoingOrders(Customer.OrgID, Customer.CustIDType,
                                    0) THEN DO:
             ASSIGN
                llOrderAllowed = FALSE
                lcReason = "PRO migration not possible because of active non pro orders".
             END.
             ELSE DO:
                llOnlyActiveFound = FALSE.             
                /* Check that customer got at least one 3P convergent
                   and all convergent subscriptions are commercially
                   active webstatus */
                FOR EACH Mobsub NO-LOCK WHERE 
                         Mobsub.Brand EQ gcBrand AND 
                         Mobsub.InvCust EQ Customer.CustNum AND
                         fIsConvergent3POnly(Mobsub.clitype):
                   FIND FIRST Clitype WHERE
                              Clitype.brand EQ "1" AND
                              Clitype.clitype EQ Mobsub.clitype NO-LOCK NO-ERROR.
                   IF (AVAIL CLitype AND clitype.WebStatusCode EQ 1) AND
                      NOT fHasTVService(Mobsub.msseq) THEN DO:
                      llOnlyActiveFound = TRUE.
                   END.
                   ELSE DO:
                      llOnlyActiveFound = FALSE.
                      LEAVE.
                   END.                  
                END.
                IF NOT llOnlyActiveFound THEN DO:
                   ASSIGN
                      llOrderAllowed = FALSE
                      lcReason = "This migration is not allowed. Please make a previous STC to an open active tariff.".
                END.
                ELSE DO:
                   /* Check that mobile subscriptions are commercially active
                      or possible to migrate to active according to defined
                      mapping */
                   FOR EACH Mobsub NO-LOCK WHERE
                            Mobsub.Brand EQ gcBrand AND
                            Mobsub.InvCust EQ Customer.CustNum AND
                            NOT fIsConvergent3POnly(Mobsub.clitype):
                      FIND FIRST Clitype WHERE
                                 Clitype.brand EQ "1" AND
                                 Clitype.clitype EQ Mobsub.clitype NO-LOCK NO-ERROR.
                      IF (AVAIL CLitype AND clitype.WebStatusCode EQ 1 OR
                         fgetActiveReplacement(Mobsub.clitype) > "") AND
                         NOT fHasTVService(Mobsub.msseq) THEN DO:
                         llOnlyActiveFound = TRUE.
                      END.
                      ELSE DO:
                         /* found subscription that rejects migration 
                            commercially non active that does not have 
                            migration mapping or tv service activated */
                         llOnlyActiveFound = FALSE.
                         LEAVE.
                      END.                   
                   END.
                   IF NOT llOnlyActiveFound THEN DO:
                      ASSIGN
                         llOrderAllowed = FALSE
                         lcReason = "This migration is not allowed. Please make a previous STC to an open active tariff.".
                   END.

                END.
             END.
          END.
          ELSE
          ASSIGN
             llOrderAllowed = FALSE
             lcReason = "PRO migration not possible because of multiple mobile lines".
      ELSE IF AVAIL MobSub THEN
      DO:
         IF fCheckOngoingOrders(Customer.OrgID, Customer.CustIDType,
                                mobsub.msseq) THEN DO:
            ASSIGN
               llOrderAllowed = FALSE
               lcReason = "PRO migration not possible because of active non pro orders".
         END.         
         ELSE IF Mobsub.paytype THEN
            ASSIGN
               llOrderAllowed = FALSE
               lcReason = "PRO migration not possible because of prepaid subscription".
         ELSE IF fHasTVService(Mobsub.msseq) THEN
            ASSIGN
               llOrderAllowed = FALSE
               lcReason = "PRO migration not possible because of TV service".
         /* Migration not possible for retired or non active convergent */
         ELSE IF fIsConvergenceTariff(Mobsub.clitype) AND
            CAN-FIND(First CliType WHERE
                           Clitype.brand EQ gcBrand AND
                           Clitype.Clitype EQ Mobsub.clitype AND
                           Clitype.webstatuscode NE {&CLITYPE_WEBSTATUSCODE_ACTIVE}) THEN
            ASSIGN
               llOrderAllowed = FALSE
               lcReason = "PRO migration not possible because of non commercial active convergent subscription".
         ELSE IF fIsFixedOnly(Mobsub.Clitype) THEN
             ASSIGN
                 llOrderAllowed = FALSE
                 lcReason = "PRO migration not possible because of fixed only".
         ELSE IF NOT llNonProToProMigrationOngoing THEN
         DO:
             /* There exists only 1 non-pro mobile subscription, so this is for blocking migrating of non-pro mobile line to pro mobile line */
             FIND FIRST CliType WHERE CliType.Brand = gcBrand AND CliType.CliType = pcCliType NO-LOCK NO-ERROR.
             IF AVAIL CliType AND CliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
                 ASSIGN
                     llOrderAllowed = FALSE
                     lcReason = "Mobile line for non-pro customer from PRO channel".
         END.
      END.
      ELSE IF NOT llNonProToProMigrationOngoing AND
              NOT fIsConvergent3POnly(pcCliType) THEN
         ASSIGN
             llOrderAllowed = FALSE
             lcReason = "PRO migration not possible because of no mobile lines exists".
   END.
END.


FIND FIRST Customer NO-LOCK WHERE
           Customer.Brand      = gcBrand    AND
           Customer.OrgID      = pcPersonId AND
           Customer.CustIDType = pcIdType   AND
           Customer.Roles     NE "inactive" NO-ERROR.

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

ASSIGN
    lcPROChannels    = fCParamC("PRO_CHANNELS").

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
ELSE IF AVAIL Customer AND
   CAN-FIND(FIRST MobSub WHERE 
                  Mobsub.Brand EQ gcBrand AND
                  Mobsub.InvCust EQ Customer.CustNum) THEN DO:
   FIND FIRST CustCat WHERE Custcat.brand EQ "1" AND CustCat.category EQ Customer.category NO-LOCK NO-ERROR.
   IF AVAIL CustCat THEN
      ASSIGN
          llCustCatPro = CustCat.pro
          lcSegment    = CustCat.Segment.
 
    ASSIGN 
       llNonProToProMigrationOngoing = fCheckOngoingProMigration   (Customer.CustNum)
       llProToNonProMigrationOngoing = fCheckOngoingNonProMigration(Customer.CustNum). 

    IF LOOKUP(pcChannel,lcPROChannels) > 0 THEN 
    DO:
        IF LOOKUP(pcIdType,"NIF,NIE") > 0 AND NOT plSelfEmployed THEN
           ASSIGN
              llOrderAllowed = FALSE
              lcReason = "PRO migration not possible because of not company or selfemployed". 
        ELSE IF llCustCatPro THEN 
        DO:            
            IF NOT fIsConvergent3POnly(pcCliType) AND 
               (NOT (fCheckExistingConvergentWithoutALCheck (pcIdType, pcPersonId, pcCliType) OR 
                     fCheckOngoingConvergentOrderWithoutALCheck(pcIdType, pcPersonId, pcCliType))) THEN
                ASSIGN 
                    llOrderAllowed = FALSE
                    lcReason       = "Additional mobile line is not compatible with respective to main convergent line".
            ELSE IF llProToNonProMigrationOngoing THEN  
                ASSIGN
                    llOrderAllowed = FALSE
                    lcReason       = "Ongoing non PRO order".
        END.
        ELSE 
        DO: /* NOT llCustCatPro */
            IF plSTCMigrate OR llNonProToProMigrationOngoing THEN
               fCheckMigration().
            ELSE
               ASSIGN
                  llOrderAllowed = FALSE
                  lcReason = "Non PRO customer in PRO channel without migrate".
        END.
    END.
    ELSE 
    DO:
        IF llCustCatPro THEN 
        DO:
            IF plSTCMigrate THEN 
            DO:
                fCheckMigration().
                FIND Mobsub WHERE Mobsub.Brand EQ gcBrand AND Mobsub.InvCust EQ Customer.CustNum NO-LOCK NO-ERROR.
                IF AVAIL MobSub AND NOT llProToNonProMigrationOngoing THEN 
                    ASSIGN 
                        llOrderAllowed = FALSE
                        lcReason = "PRO migration not possible because of mobile line".
                ELSE IF NOT llProToNonProMigrationOngoing THEN 
                    ASSIGN 
                        llOrderAllowed = FALSE
                        lcReason = "PRO migration not possible because, no mobile lines exists".
            END.
            ELSE
            ASSIGN
               llOrderAllowed = FALSE
               lcReason = "PRO customer in non-PRO channel".
        END.
        ELSE
        DO:
           IF llNonProToProMigrationOngoing THEN 
              ASSIGN
                 llOrderAllowed = FALSE
                 lcReason = "Customer already exists with PRO category".
           ELSE IF (pcChannel EQ "Newton" OR pcChannel EQ "VFR") AND
                   plSTCMigrate THEN DO:
              fCheckMigration().
           END.
        END.
    END.
END.
ELSE DO:
   lcSegment = fgetCustSegment(pcIdType, plSelfEmployed, llProChannel, OUTPUT lccategory).
   IF LOOKUP(pcChannel,lcPROChannels) > 0 THEN DO:
      IF LOOKUP(pcIdType,"NIF,NIE") > 0 AND NOT plSelfEmployed THEN
           ASSIGN
              llOrderAllowed = FALSE
              lcReason = "PRO migration not possible because of not company or selfemployed". 
      ELSE DO:
         FOR EACH OrderCustomer WHERE
                  OrderCustomer.Brand      EQ gcBrand    AND
                  OrderCustomer.CustIdType EQ pcIdType   AND
                  OrderCustomer.CustId     EQ pcPersonId NO-LOCK:

             IF OrderCustomer.PRO EQ FALSE THEN 
             DO:    
                FIND FIRST Order WHERE Order.Brand EQ gcBrand AND Order.OrderId EQ OrderCustomer.OrderId NO-LOCK NO-ERROR.
                IF AVAIL Order AND LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 THEN 
                DO:
                    llOrderAllowed = FALSE.
                    lcReason = "Ongoing non PRO order".
                    LEAVE.    
                END.
             END.
             ELSE 
                ASSIGN llPROOngoingOrder = TRUE.
         END.

         /* Assume, there is no ongoing order for customer selected from PRO channels */
         IF NOT llPROOngoingOrder AND NOT llCustCatPro THEN 
         DO:
             FIND FIRST CliType WHERE CliType.Brand = gcBrand AND CliType.CliType = pcCliType NO-LOCK NO-ERROR.
             IF AVAIL CliType AND CliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
             DO:
                 llOrderAllowed = FALSE.
                 lcReason = "Mobile line for non-pro customer from PRO channel".
             END.
         END.
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

/* Check extra lines discount is allowed for customer */
lcExtraLineAllowed = "".

IF fCheckExistingConvergentAvailForExtraLine(pcIdType,pcPersonId,OUTPUT liMainLineOrderId) THEN 
   lcExtraLineAllowed = "OK".
ELSE IF fCheckOngoingConvergentAvailForExtraLine(pcIdType,pcPersonId,OUTPUT liOngoingOrderId) THEN    
   lcExtraLineAllowed = "OK".
ELSE lcExtraLineAllowed = "NO_MAIN_LINE".   

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

IF lcExtraLineAllowed EQ "" THEN lcExtraLineAllowed = "NO_SUBSCRIPTIONS".

lcReturnStruct = add_struct(response_toplevel_id, "").
add_boolean(lcReturnStruct, 'order_allowed', llOrderAllowed).
add_int(lcReturnStruct, 'subscription_limit', liSubLimit).
IF NOT llOrderAllowed THEN add_string(lcReturnStruct, 'reason',lcReason).
add_string(lcReturnStruct, 'additional_line_allowed',lcAddLineAllowed).
add_string(lcReturnStruct, 'extra_line_allowed',lcExtraLineAllowed).

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
