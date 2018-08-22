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
                   reasons;string;optional;possible fail reasons, returned if order_allowed = false and are | seperated reasons                   
                   additional_line_allowed;string;mandatory;OK,NO_MAIN_LINE,NO_SUBSCRIPTIONS (OK is returned also if there's no active main line but a pending main line order)
                   extra_line_allowed;string;mandatory;comma separated list of allowed extra lines
                   segment;string;mandatory;
                   extra_line_status;integer;mandatory;status for extra lines orders (YCO-272). Only make sense if asking for a Extra Line CliType.
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/tmsconst.i}
{Func/custfunc.i}
{Func/profunc.i}
{Func/barrfunc.i}
{Func/profunc_request.i}
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
DEF VAR lii                           AS INT  NO-UNDO.
DEF VAR lcExtraLineCLITypes           AS CHAR NO-UNDO.
DEF VAR liMLMsSeq                     AS INT  NO-UNDO. 
DEF VAR lcReasons                     AS CHAR NO-UNDO.
DEF VAR llAllowedELCliTypeCurrent     AS LOG  NO-UNDO.  
DEF VAR llAllowedELCliTypeOther       AS LOG  NO-UNDO. 
DEF VAR lcClitypeAux                  AS CHAR NO-UNDO.
DEF VAR liExtraLineStatus             AS INT  NO-UNDO.
DEF VAR llAllowedProMig               AS LOGI NO-UNDO.
DEF VAR lcCliTypeTo                   AS CHAR NO-UNDO.

DEF BUFFER bCustomer  FOR Customer.
DEF BUFFER bMobSub    FOR MobSub.
DEF BUFFER bCustCat   FOR Custcat.
DEF BUFFER bOrder     FOR Order.
DEF BUFFER bClitype   FOR CLIType.
DEF VARIABLE llHasLegacyTariff AS LOGICAL NO-UNDO.
   
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

FUNCTION fSetError RETURNS LOG (INPUT icError AS CHARACTER):
      ASSIGN
         llOrderAllowed = FALSE
         lcReason       = icError
         lcReasons      = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.  
      RETURN TRUE .
END FUNCTION .        

/* YCO-712                              */
/* Returns TRUE if any subscription of  */
/* this customer has TV Service active. */      
FUNCTION fTVService RETURNS LOGICAL ():
   DEFINE BUFFER bMobSub FOR MobSub.
   FOR EACH bMobsub NO-LOCK WHERE 
            bMobsub.Brand   EQ Syst.Var:gcBrand AND 
            bMobsub.AgrCust EQ Customer.CustNum:
      IF fIsTVServiceActive(bMobsub.MsSeq) THEN 
        RETURN TRUE.            
   END.           
   RETURN FALSE.
END FUNCTION.

FUNCTION fCheckMigration RETURNS LOG ():

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.

   DEF VAR llOnlyActiveFound AS LOG NO-UNDO.

   IF LOOKUP(pcIdType,"NIF,NIE") > 0 AND (NOT plSelfEmployed) AND (NOT fTVService()) /* YCO-712 */
      THEN
       fSetError ("PRO migration not possible because of not company or selfemployed") .
   ELSE DO:
      FIND Mobsub WHERE Mobsub.Brand EQ Syst.Var:gcBrand AND Mobsub.InvCust EQ Customer.CustNum NO-LOCK NO-ERROR.
      IF AMBIG MobSub THEN
          IF (pcChannel EQ "Newton" OR pcChannel EQ "VFR") THEN DO:
             IF CAN-FIND(FIRST Mobsub NO-LOCK WHERE
                               Mobsub.Brand EQ Syst.Var:gcBrand AND
                               Mobsub.InvCust EQ Customer.CustNum AND
                               Mobsub.paytype) THEN DO:
               fSetError ("PRO migration not possible because of prepaid subscription").
             END.
             /* Check any ongoing orders */
             ELSE IF Func.ValidateACC:mCheckOngoingOrders(Customer.OrgID, Customer.CustIDType,
                                    0) THEN DO:
                   fSetError ("PRO migration not possible because of active non pro orders").
             END.
             ELSE 
             DO:
                FOR EACH Mobsub NO-LOCK WHERE 
                         Mobsub.Brand   EQ Syst.Var:gcBrand AND 
                         Mobsub.AgrCust EQ Customer.CustNum:

                   IF Mobsub.IMSI EQ "" THEN
                   DO:
                      fSetError ("PRO migration not possible because of fixed only").
                      LEAVE.
                   END.     

                   /* Check all subscriptions of this customer are possible to migrate per defined mapping */ 
                   llAllowedProMig = fCheckSubscriptionTypeAllowedForProMigration(MobSub.CliType, OUTPUT lcClitypeTo).
                   IF NOT llAllowedProMig THEN 
                   DO:
                      fSetError ("PRO migration not possible, as mapping to change tariff to the commercially active pro tariff is missing.").
                      LEAVE.        
                   END.

                   /* YCO-712. "PRO migration not possible because of TV service" removed. */

                END.
             END.
          END.
          ELSE IF (pcChannel EQ "Telesales_pro" OR
                   pcChannel EQ "Fusion_Telesales_pro" OR 
                   pcChannel EQ "Fusion_pos_pro") 
          THEN DO:
             IF CAN-FIND(FIRST Mobsub NO-LOCK WHERE
                               Mobsub.Brand EQ Syst.Var:gcBrand AND
                               Mobsub.InvCust EQ Customer.CustNum AND
                               Mobsub.paytype) THEN DO:
               fSetError ("PRO migration not possible because of prepaid subscription").
             END.
             /* Check any ongoing orders */
             ELSE IF Func.ValidateACC:mCheckOngoingOrders(Customer.OrgID, Customer.CustIDType,
                                    0) THEN DO:
             ASSIGN
                llOrderAllowed = FALSE
                lcReason = "PRO migration not possible because of active non pro orders"
                lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
             END.
             ELSE 
             DO:
                FOR EACH Mobsub NO-LOCK WHERE 
                         Mobsub.Brand   EQ Syst.Var:gcBrand AND 
                         Mobsub.AgrCust EQ Customer.CustNum:

                   IF Mobsub.IMSI EQ "" THEN
                   DO:
                      fSetError ("PRO migration not possible because of fixed only").
                      LEAVE.
                   END.     

                   /* Check all subscriptions of this customer are possible to migrate per defined mapping */ 
                   llAllowedProMig = fCheckSubscriptionTypeAllowedForProMigration(MobSub.CliType, OUTPUT lcClitypeTo).
                   IF NOT llAllowedProMig THEN 
                   DO:
                      fSetError ("PRO migration not possible, as mapping to change tariff to the commercially active pro tariff is missing.").
                      LEAVE.        
                   END.

                   /* YCO-712. "PRO migration not possible because of TV service" removed. */

                END.                
             END.             
          END.          
          ELSE
              fSetError ("PRO migration not possible because of multiple mobile lines") .
      ELSE IF AVAIL MobSub THEN
      DO:
         IF Func.ValidateACC:mCheckOngoingOrders(Customer.OrgID, Customer.CustIDType,
                                mobsub.msseq) THEN DO:
               fSetError ("PRO migration not possible because of active non pro orders").
         END.         
         ELSE IF Mobsub.paytype THEN
               fSetError ("PRO migration not possible because of prepaid subscription" ).
               
         /* YCO-712. "PRO migration not possible because of TV service" removed. */
         
         /* Migration not possible for retired or non active convergent */
         ELSE IF fIsConvergenceTariff(Mobsub.clitype) AND
            CAN-FIND(First CliType WHERE
                           Clitype.brand EQ Syst.Var:gcBrand AND
                           Clitype.Clitype EQ Mobsub.clitype AND
                           Clitype.webstatuscode NE {&CLITYPE_WEBSTATUSCODE_ACTIVE}) THEN
            ASSIGN
               llOrderAllowed = FALSE
               lcReason = "PRO migration not possible because of non commercial active convergent subscription"
               lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
         ELSE IF Mobsub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE} THEN
             ASSIGN
                 llOrderAllowed = FALSE
                 lcReason = "PRO migration not possible because of fixed only"
                 lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
         ELSE IF NOT llNonProToProMigrationOngoing THEN
         DO:
             /* There exists only 1 non-pro mobile subscription, so this is for blocking migrating of non-pro mobile line to pro mobile line */
             IF NOT fIsConvergenceTariff(pcCliType) THEN
                 ASSIGN
                     llOrderAllowed = FALSE
                     lcReason = "Mobile line for non-pro customer from PRO channel"
                     lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
         END.
      END.
      ELSE IF NOT llNonProToProMigrationOngoing AND
              NOT fIsConvergent3POnly(pcCliType) THEN
         ASSIGN
             llOrderAllowed = FALSE
             lcReason = "PRO migration not possible because of no mobile lines exists"
             lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
   END.
END.

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
   llOrderAllowed = FALSE
   lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.

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
      lcReason = "The customer must have self employed set to true"
      lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
END.
ELSE IF AVAIL Customer AND
   CAN-FIND(FIRST MobSub WHERE 
                  Mobsub.Brand EQ Syst.Var:gcBrand AND
                  Mobsub.InvCust EQ Customer.CustNum) THEN DO:
   FIND FIRST CustCat WHERE Custcat.brand EQ "1" AND CustCat.category EQ Customer.category NO-LOCK NO-ERROR.
   IF AVAIL CustCat THEN
      ASSIGN
          llCustCatPro = CustCat.pro.
 
    ASSIGN 
       llNonProToProMigrationOngoing = fCheckOngoingProMigration   (Customer.CustNum)
       llProToNonProMigrationOngoing = fCheckOngoingNonProMigration(Customer.CustNum). 

    IF LOOKUP(pcChannel,lcPROChannels) > 0 THEN 
    DO:
       IF LOOKUP(pcIdType,"NIF,NIE") > 0 AND (NOT plSelfEmployed) AND (NOT fTVService())  /* YCO-712 */
       THEN
          ASSIGN
             llOrderAllowed = FALSE
             lcReason = "PRO migration not possible because of not company or selfemployed"
             lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
        ELSE IF llCustCatPro THEN 
        DO:            
            IF NOT fIsConvergent3POnly(pcCliType) AND 
               (NOT (fCheckExistingConvergentWithoutALCheck (pcIdType, pcPersonId, pcCliType) OR 
                     fCheckOngoingConvergentOrderWithoutALCheck(pcIdType, pcPersonId, pcCliType))) THEN
                ASSIGN 
                    llOrderAllowed = FALSE
                    lcReason       = "Additional mobile line is not compatible with respective to main convergent line"
                    lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
            ELSE IF llProToNonProMigrationOngoing THEN  
                ASSIGN
                    llOrderAllowed = FALSE
                    lcReason       = "Ongoing non PRO order"
                    lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
        END.
        ELSE 
        DO: /* NOT llCustCatPro */
            IF plSTCMigrate OR llNonProToProMigrationOngoing THEN
               fCheckMigration().
            ELSE
               ASSIGN
                  llOrderAllowed = FALSE
                  lcReason = "Non PRO customer in PRO channel without migrate"
                  lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
        END.
    END.
    ELSE 
    DO:
        IF llCustCatPro THEN 
        DO:
            IF plSTCMigrate THEN 
            DO:
                fCheckMigration().
                FIND Mobsub WHERE Mobsub.Brand EQ Syst.Var:gcBrand AND Mobsub.InvCust EQ Customer.CustNum NO-LOCK NO-ERROR.
                IF AVAIL MobSub AND NOT llProToNonProMigrationOngoing THEN 
                    ASSIGN 
                        llOrderAllowed = FALSE
                        lcReason = "PRO migration not possible because of mobile line"
                        lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
                ELSE IF NOT llProToNonProMigrationOngoing THEN 
                    ASSIGN 
                        llOrderAllowed = FALSE
                        lcReason = "PRO migration not possible because, no mobile lines exists"
                        lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
            END.
            ELSE
            ASSIGN
               llOrderAllowed = FALSE
               lcReason = "PRO customer in non-PRO channel"
               lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
        END.
        ELSE
        DO:
           IF llNonProToProMigrationOngoing THEN 
              ASSIGN
                 llOrderAllowed = FALSE
                 lcReason = "Customer already exists with PRO category"
                 lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
           ELSE IF (pcChannel EQ "Newton" OR pcChannel EQ "VFR") AND
                   plSTCMigrate THEN DO:
              fCheckMigration().
           END.
        END.
    END.
END.
ELSE 
DO:
   IF LOOKUP(pcChannel,lcPROChannels) > 0 THEN 
   DO:
       IF LOOKUP(pcIdType,"NIF,NIE") > 0 AND (NOT plSelfEmployed) AND (NOT fTVService())  /* YCO-712 */
       THEN
          ASSIGN
             llOrderAllowed = FALSE
             lcReason = "PRO migration not possible because of not company or selfemployed"
             lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
       ELSE 
       DO:
         FOR EACH OrderCustomer NO-LOCK WHERE
                  OrderCustomer.Brand      EQ Syst.Var:gcBrand    AND
                  OrderCustomer.CustIdType EQ pcIdType   AND
                  OrderCustomer.CustId     EQ pcPersonId AND
                  OrderCustomer.Rowtype    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
            FIRST Order NO-LOCK WHERE
                  Order.Brand EQ Syst.Var:gcBrand AND
                  Order.OrderID = OrderCustomer.OrderID:

             IF Order.OrderType NE {&ORDER_TYPE_NEW} AND
                Order.OrderType NE {&ORDER_TYPE_MNP} AND
                Order.OrderType NE {&ORDER_TYPE_STC} THEN NEXT.

             IF OrderCustomer.PRO EQ FALSE THEN 
             DO:    
                IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 THEN 
                DO:
                    llOrderAllowed = FALSE.
                    lcReason = "Ongoing non PRO order".
                    lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
                    LEAVE.    
                END.
             END.
             ELSE IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 THEN
                ASSIGN llPROOngoingOrder = TRUE.
         END.

         /* Assume, there is no ongoing order for customer selected from PRO channels */
         IF NOT llPROOngoingOrder AND NOT llCustCatPro THEN 
         DO:
             IF NOT fIsConvergenceTariff(pcCliType) THEN
             DO:
                 llOrderAllowed = FALSE.
                 lcReason = "Mobile line for non-pro customer from PRO channel".
                 lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
             END.
         END.
       END.
   END.
   ELSE 
   DO: /* Block nonpro order's from non-pro channels when pro order is ongoing */
      FOR EACH OrderCustomer NO-LOCK WHERE
               OrderCustomer.Brand      EQ Syst.Var:gcBrand    AND
               OrderCustomer.CustIdType EQ pcIdType   AND
               OrderCustomer.CustId     EQ pcPersonId AND
               OrderCustomer.Rowtype    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
         FIRST Order NO-LOCK WHERE
               Order.Brand EQ Syst.Var:gcBrand AND
               Order.OrderID = OrderCustomer.OrderID:

          IF Order.OrderType NE {&ORDER_TYPE_NEW} AND
             Order.OrderType NE {&ORDER_TYPE_MNP} AND
             Order.OrderType NE {&ORDER_TYPE_STC} THEN NEXT.

           IF OrderCustomer.PRO THEN 
           DO:    
              IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 THEN 
              DO:
                  llOrderAllowed = FALSE.
                  lcReason = "Ongoing PRO order".
                  lcReasons = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.
                  LEAVE.    
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

IF llOrderAllowed = NO               AND 
   lcReason = "subscription limit"   AND
   fCLITypeIsExtraLine(pcCliType)    AND 
   LOOKUP(pcCliType,lcExtraLineAllowed) > 0 THEN
   ASSIGN 
       lcReason        = ""
       llOrderAllowed  = TRUE.
       
IF fCLITypeIsExtraLine(pcCliType)           AND
   LOOKUP(pcCliType,lcExtraLineAllowed) = 0 THEN
   ASSIGN llOrderAllowed = FALSE.

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

YCO-272: /* When asking about a new Extra Line... */
DO: 
   liExtraLineStatus = -1. /* Initializing variable */      
   IF LOOKUP(pcCliType, lcExtraLineCliTypes) > 0 THEN DO: 
      
      /* Check if current extra line CliType is allowed for this customer */
      llAllowedELCliTypeCurrent = fELCliTypeAllowedForCustomer (INPUT pcIdType,
                                                                INPUT pcPersonId,
                                                                INPUT pcCliType).
   
      /* Check if other extra line CliType is allowed for this customer */
      llAllowedELCliTypeOther = FALSE.
      DO lii = 1 TO NUM-ENTRIES(lcExtraLineCliTypes):
         lcCliTypeAux = TRIM(ENTRY(lii,lcExtraLineCliTypes)).
         IF lcCliTypeAux = pcCliType THEN 
           NEXT.
         llAllowedELCliTypeOther = fELCliTypeAllowedForCustomer (INPUT pcIdType,
                                                                 INPUT pcPersonId,
                                                                 INPUT lcCliTypeAux).
         IF llAllowedELCliTypeOther THEN
           LEAVE.                                                                           
      END.               
      
      /* Cases */
      IF LOOKUP(pcCliType, lcExtraLineAllowed) > 0 THEN DO:
         liExtraLineStatus = 0. /* Extra Line allowed. Go on. */
         LEAVE YCO-272.         
      END.               
      IF (NOT llAllowedELCliTypeCurrent) AND (NOT llAllowedELCliTypeOther) THEN DO:
         liExtraLineStatus = 1. /* No subscriptions compatible with Extra Lines */
         fSetError ("Extra Line not allowed").
         LEAVE YCO-272.         
      END.               
      IF lcExtraLineAllowed = "" THEN DO:
         liExtraLineStatus = 2. /* No more Extra Lines allowed */
         fSetError ("Extra Line not allowed").         
         LEAVE YCO-272.         
      END.                 
      IF lcExtraLineAllowed <> "" AND (NOT llAllowedELCliTypeCurrent) THEN DO:
         liExtraLineStatus = 3. /* Extra Line allowed, but no this one that is not compatible */
         fSetError ("Extra Line not allowed").        
         LEAVE YCO-272.         
      END.                                                
      IF lcExtraLineAllowed <> "" AND LOOKUP(pcCliType, lcExtraLineAllowed) = 0 THEN DO:
         IF LOOKUP(pcCliType, lcExtraLineCliTypes) = 1 THEN 
           liExtraLineStatus = 5. /* Not allowed itself, but you can try other one of the allowed */
         ELSE
           liExtraLineStatus = 4. /* Not allowed itself, but you can try other one of the allowed */
         fSetError ("Extra Line not allowed").
         LEAVE YCO-272.             
      END.                                                                                       
   END. /* IF AVAILABLE Customer AND LOOKUP(pcCliType, lcExtraLineCliTypes) > 0 */
   
END. /* YCO-272 */

lcReturnStruct = add_struct(response_toplevel_id, "").
add_boolean(lcReturnStruct, 'order_allowed', llOrderAllowed).
add_int(lcReturnStruct, 'subscription_limit', liSubLimit).
IF NOT llOrderAllowed THEN add_string(lcReturnStruct, 'reason',lcReason).
IF NOT llOrderAllowed THEN add_string(lcReturnStruct, 'reasons',lcReasons).
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

add_int(lcReturnStruct, 'extra_line_status',liExtraLineStatus).

FINALLY:
   END.
