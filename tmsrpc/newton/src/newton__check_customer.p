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

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/orderchk.i}
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
DEF VAR lcExtraLineCliTypeListAllowed AS CHAR NO-UNDO. 
DEF VAR llNonProToProMigrationOngoing AS LOGI NO-UNDO.
DEF VAR llProToNonProMigrationOngoing AS LOGI NO-UNDO.
DEF VAR lcResult                      AS CHAR NO-UNDO.
DEF VAR lcReasons                     AS CHAR NO-UNDO. 
DEF VAR lcCurrentSegment              AS CHAR NO-UNDO.
DEF VAR liExtraLineStatus             AS INTE NO-UNDO.

DEF BUFFER bCustomer  FOR Customer.
DEF BUFFER bMobSub    FOR MobSub.
DEF BUFFER bCustCat   FOR Custcat.
DEF BUFFER bOrder     FOR Order.
DEF BUFFER bClitype   FOR CLIType.

FUNCTION fSetError RETURNS LOG (INPUT icError AS CHARACTER):
   ASSIGN
      llOrderAllowed = FALSE
      lcReason       = icError
      lcReasons      = lcReasons + ( IF lcReasons NE "" THEN "|" ELSE "" ) + lcReason.  
   RETURN TRUE.
END FUNCTION .        

FUNCTION fCheckMigration RETURNS LOG 
   ():

    DEF BUFFER Order         FOR Order.
    DEF BUFFER OrderCustomer FOR OrderCustomer.

    DEF VAR llAllowedProMig   AS LOGI NO-UNDO.
    DEF VAR lcCliTypeTo       AS CHAR NO-UNDO.

    IF CAN-FIND(FIRST Mobsub NO-LOCK WHERE
                      Mobsub.Brand   EQ Syst.Var:gcBrand AND
                      Mobsub.AgrCust EQ Customer.CustNum AND
                      Mobsub.paytype) THEN
       fSetError ("PRO migration not possible because of prepaid subscription").
    /* Check any ongoing orders */
    ELSE IF fCheckOngoingOrders(Customer.OrgId, Customer.CustIdType, 0) THEN
       fSetError ("PRO migration not possible because of active ongoing orders").
    ELSE 
    DO:
        FIND Mobsub WHERE Mobsub.Brand EQ Syst.Var:gcBrand AND Mobsub.AgrCust EQ Customer.CustNum NO-LOCK NO-ERROR.
        IF AMBIG MobSub THEN
        DO:    
           FOR EACH Mobsub NO-LOCK WHERE 
                    Mobsub.Brand   EQ Syst.Var:gcBrand AND 
                    Mobsub.AgrCust EQ Customer.CustNum:

              IF fIsFixedOnly(Mobsub.Clitype) THEN
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

              /* TV service not allowed for PRO */    
              IF NOT fIsConvergent3POnly(Mobsub.clitype) THEN 
                 NEXT.
              IF fHasTVService(Mobsub.msseq) THEN 
              DO:   
                 fSetError ("PRO migration not possible because of TV service").
                 LEAVE.
              END.
           END.
        END.        
        ELSE IF AVAIL MobSub THEN
        DO:
           IF fIsFixedOnly(Mobsub.Clitype) THEN
              fSetError ("PRO migration not possible because of fixed only").
           ELSE IF fHasTVService(Mobsub.msseq) THEN
              fSetError ("PRO migration not possible because of TV service").
           ELSE
           DO:
              llAllowedProMig = fCheckSubscriptionTypeAllowedForProMigration(MobSub.CliType, OUTPUT lcClitypeTo).
              IF NOT llAllowedProMig THEN 
              DO:
                 fSetError ("PRO migration not possible, as mapping to change tariff to the commercially active pro tariff is missing.").
                 LEAVE.        
              END.
           END.
        END.
        ELSE 
           fSetError ("PRO migration not possible because of no mobile lines exists").
    END.                 
  
    RETURN TRUE.

END FUNCTION.


FUNCTION fGetAddLineAllowed RETURNS CHARACTER():

   IF LOOKUP(pcCliType,{&ADDLINE_CLITYPES}) > 0 THEN 
   DO:
       IF fCheckExistingConvergent(pcIdType,pcPersonId,pcCliType) THEN
          RETURN "OK". 
       ELSE IF fCheckOngoingConvergentOrder(pcIdType,pcPersonId,pcCliType) THEN 
          RETURN "OK".
       ELSE IF fCheckExistingMobileOnly(pcIdType,pcPersonId,pcCliType) THEN 
          RETURN "MOBILE_ONLY". /* Additional Line with mobile only ALFMO-5 */
       ELSE IF fCheckOngoingMobileOnly(pcIdType,pcPersonId,pcCliType) THEN 
          RETURN "MOBILE_ONLY". /* Additional Line with mobile only ALFMO-5 */ 
       ELSE 
          RETURN "NO_MAIN_LINE".
   END.
   ELSE 
   DO:
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
                           CLIType.Brand    = Syst.Var:gcBrand          AND
                           CLIType.CLIType  = OrderAction.ItemKey       AND
                           CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN 
            RETURN "OK".  
      END.

   END.
   
   RETURN "NO_SUBSCRIPTIONS".

END FUNCTION.


FUNCTION fGetExtraLineAllowed RETURNS CHARACTER
   (OUTPUT ocCliTypeList AS CHAR):

   DEF VAR lcCliTypesAllowed AS CHAR NO-UNDO.
   DEF VAR lcCliType         AS CHAR NO-UNDO.
   DEF VAR liMLMsSeq         AS INT  NO-UNDO. 
   DEF VAR lii               AS INT  NO-UNDO. 

   ASSIGN ocCliTypeList = fExtraLineCLITypes().

   DO lii = 1 TO NUM-ENTRIES(ocCliTypeList):

      ASSIGN lcCliType = ENTRY(lii,ocCliTypeList).

      IF fCheckExistingMainLineAvailForExtraLine(lcCliType,pcIdType,pcPersonId,OUTPUT liMLMsSeq) > 0 OR
         fCheckOngoingMainLineAvailForExtraLine (lcCliType,pcIdType,pcPersonId) > 0 THEN 
         lcCliTypesAllowed = lcCliTypesAllowed + (IF lcCliTypesAllowed <> "" THEN "," ELSE "") + lcCliType.
   END.

   IF lcCliTypesAllowed EQ "" THEN 
      lcCliTypesAllowed = "NO_MAIN_LINE".

   RETURN lcCliTypesAllowed.

END FUNCTION.


FUNCTION fCheckAnyOtherExtraLineCliTypeAllowedForCustomer RETURNS LOGICAL
   (INPUT icCliTypeList AS CHARACTER,
    INPUT icCliType     AS CHARACTER,
    INPUT icCustIdType  AS CHARACTER,
    INPUT icCustId      AS CHARACTER):

   DEF VAR lii          AS INTE NO-UNDO.
   DEF VAR lcClitypeAux AS CHAR NO-UNDO.
   DEF VAR llAllowed    AS LOGI NO-UNDO.

   DO lii = 1 TO NUM-ENTRIES(icCliTypeList):

      lcCliTypeAux = TRIM(ENTRY(lii,icCliTypeList)).
      IF lcCliTypeAux = icCliType THEN 
         NEXT.

      llAllowed = fELCliTypeAllowedForCustomer (INPUT icCustIdType,
                                                INPUT icCustId,
                                                INPUT lcCliTypeAux).
      IF llAllowed THEN
         LEAVE.                                                                           
   END.

   RETURN llAllowed.

END FUNCTION.   


FUNCTION fGetExtraLineStatus RETURNS INTEGER 
   (INPUT icCustIdType                  AS CHAR,
    INPUT icCustId                      AS CHAR,
    INPUT icNewCliType                  AS CHAR,
    INPUT icAllowedExtraLineCliTypeList AS CHAR,
    INPUT icAllExtralineCliTypeList     AS CHAR):

   DEF VAR llAllowedELCliTypeCurrent     AS LOG  NO-UNDO.  
   DEF VAR llAllowedELCliTypeOther       AS LOG  NO-UNDO.

   IF LOOKUP(icNewCliType, icAllExtralineCliTypeList) > 0 THEN 
   DO:    
      /* Check if current extra line CliType is allowed for this customer */
      llAllowedELCliTypeCurrent = fELCliTypeAllowedForCustomer (INPUT icCustIdType,
                                                                INPUT icCustId,
                                                                INPUT icNewCliType).
      /* Check if other extra line CliType is allowed for this customer */
      llAllowedELCliTypeOther = fCheckAnyOtherExtraLineCliTypeAllowedForCustomer(icAllExtralineCliTypeList, 
                                                                                 icNewCliType, 
                                                                                 icCustIdType, 
                                                                                 icCustId).
      /* Cases */
      IF (NOT llAllowedELCliTypeCurrent) AND (NOT llAllowedELCliTypeOther) THEN 
      DO:
         fSetError("Extra Line not allowed").
         RETURN 1. /* No subscriptions compatible with Extra Lines */
      END.
      ELSE IF icAllowedExtraLineCliTypeList = "" THEN
      DO:               
         fSetError ("Extra Line not allowed").         
         RETURN 2. /* No more Extra Lines allowed */
      END.
      ELSE IF icAllowedExtraLineCliTypeList <> "" AND (NOT llAllowedELCliTypeCurrent) THEN 
      DO:
         fSetError ("Extra Line not allowed").        
         RETURN 3. /* Extra Line allowed, but no this one that is not compatible */
      END.
      ELSE IF icAllowedExtraLineCliTypeList <> "" AND 
              LOOKUP(icNewCliType, icAllowedExtraLineCliTypeList) = 0 THEN 
      DO:
         fSetError ("Extra Line not allowed"). 
         IF LOOKUP(icNewCliType, icAllExtralineCliTypeList) > 1 THEN 
           RETURN 5. /* Not allowed itself, but you can try other one of the allowed */
         ELSE
           RETURN 4. /* Not allowed itself, and you can't try any other */
      END.
      ELSE IF LOOKUP(icNewCliType, icAllowedExtraLineCliTypeList) > 0 THEN 
         RETURN 0. /* Extra Line allowed. Go on. */                                                                                                      
   END. 

   RETURN -1.

END FUNCTION.


FUNCTION fGetAllowedExtralineCliTypeListAndStatus RETURN LOGICAL
  (OUTPUT ocExtraLineCliTypeListAllowed AS CHAR,
   OUTPUT oiExtraLineStatus             AS INTE):

   DEF VAR lcAllAllowedExtraLineCliTypeList AS CHAR NO-UNDO.

   ASSIGN ocExtraLineCliTypeListAllowed = fGetExtraLineAllowed(OUTPUT lcAllAllowedExtraLineCliTypeList).
       
   /* Override extra line subscription from subscription limit */
   IF llOrderAllowed = NO               AND 
      lcReason = "subscription limit"   AND
      fCLITypeIsExtraLine(pcCliType)    AND 
      LOOKUP(pcCliType,ocExtraLineCliTypeListAllowed) > 0 THEN
      ASSIGN 
         lcReason        = ""
         llOrderAllowed  = TRUE.
           
   ASSIGN oiExtraLineStatus = fGetExtraLineStatus(pcIdType, 
                                                  pcPersonId, 
                                                  pcCliType, 
                                                  ocExtraLineCliTypeListAllowed, 
                                                  lcAllAllowedExtraLineCliTypeList).
   RETURN TRUE.

END FUNCTION.

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

ASSIGN lcPROChannels = fCParamC("PRO_CHANNELS").

IF INDEX(pcChannel,"PRO") > 0 THEN 
    llProChannel = TRUE.

llOrderAllowed = fSubscriptionLimitCheck(
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
   ELSE 
      lcReason = "subscription limit".
END.

FIND FIRST Customer NO-LOCK WHERE
           Customer.Brand      = Syst.Var:gcBrand AND
           Customer.OrgID      = pcPersonId       AND
           Customer.CustIDType = pcIdType         AND
           Customer.Roles     NE "inactive"       NO-ERROR.
IF NOT AVAIL Customer THEN
   fSetError('No active Customer found').
ELSE
DO:
   FIND FIRST CustCat WHERE Custcat.Brand EQ "1" AND CustCat.category EQ Customer.category NO-LOCK NO-ERROR.
   IF NOT AVAIL CustCat THEN 
      fSetError('Customer Category not found').

   ASSIGN 
      llCustCatPro     = CustCat.Pro
      lcCurrentSegment = CustCat.Segment.

   IF NOT plSelfEmployed THEN
   DO:
      IF LOOKUP(pcIdType,"NIF,NIE") > 0 THEN
         fSetError('PRO migration not possible because of not company or selfemployed').
      ELSE IF LOOKUP(lcCurrentSegment, "COMPANY,SOHO-COMPANY") = 0 AND NOT llProChannel THEN   
         fSetError('PRO migration not possible because of not company or selfemployed').   
      ELSE IF LOOKUP(lcCurrentSegment, "AUTONOMO,SOHO-AUTONOMO") > 0 AND NOT llProChannel THEN
         fSetError('The customer must have self employed set to true').
   END.
   ELSE IF fExistBarredSubForCustomer(Customer.CustNum) THEN    
      fSetError('barring').
   ELSE IF CAN-FIND(FIRST MobSub WHERE Mobsub.Brand EQ Syst.Var:gcBrand AND Mobsub.AgrCust EQ Customer.CustNum) THEN 
   DO:
      ASSIGN 
         llNonProToProMigrationOngoing = fCheckOngoingProMigration   (Customer.CustNum)
         llProToNonProMigrationOngoing = fCheckOngoingNonProMigration(Customer.CustNum).

      IF LOOKUP(pcChannel,lcPROChannels) > 0 THEN 
      DO:
          IF llCustCatPro THEN 
          DO:            
              IF NOT fIsConvergent3POnly(pcCliType) AND 
                 (NOT (fCheckExistingConvergentWithoutALCheck (pcIdType, pcPersonId, pcCliType) OR 
                       fCheckOngoingConvergentOrderWithoutALCheck(pcIdType, pcPersonId, pcCliType))) THEN
                  fSetError('Additional mobile line is not compatible with respective to main convergent line').
              ELSE IF llProToNonProMigrationOngoing THEN  
                  fSetError('Ongoing non PRO order').        
          END.
          ELSE 
          DO: /* NOT llCustCatPro */
              IF plSTCMigrate OR llNonProToProMigrationOngoing THEN
                 fCheckMigration().
              ELSE
                 fSetError('Non PRO customer in PRO channel without migrate').
          END.
      END.
      ELSE
      DO: /* NON-PRO Channels */
         IF llCustCatPro THEN 
         DO:
            IF plSTCMigrate THEN 
            DO:
               fCheckMigration().

               /* Block, pro customer's from ordering through non pro channels, when there is no pro to non pro migration ongoing */
               FIND FIRST Mobsub WHERE Mobsub.Brand EQ Syst.Var:gcBrand AND Mobsub.AgrCust EQ Customer.CustNum NO-LOCK NO-ERROR.
               IF AVAIL MobSub AND NOT llProToNonProMigrationOngoing THEN 
                   fSetError('PRO migration not possible because of mobile line').
               ELSE IF NOT llProToNonProMigrationOngoing THEN 
                  fSetError('PRO migration not possible because, no mobile lines exists').      
            END.
            ELSE
               fSetError('PRO customer in non-PRO channel').
         END.
         ELSE
         DO:
            /* Block, non pro customer's from ordering through non pro channels, for which pro migration is ongoing */
            IF llNonProToProMigrationOngoing THEN 
               fSetError('Customer already exists with PRO category').
            ELSE IF (pcChannel EQ "Newton" OR pcChannel EQ "VFR") AND plSTCMigrate THEN
                fCheckMigration().
         END.
      END.
   END.
   ELSE
   DO:
      IF LOOKUP(pcChannel,lcPROChannels) > 0 THEN 
      DO: 
         ASSIGN llProToNonProMigrationOngoing = fCheckOngoingNonProMigration(Customer.CustNum).

         IF llProToNonProMigrationOngoing THEN 
            fSetError('Ongoing non PRO order').
         ELSE IF NOT llProToNonProMigrationOngoing AND NOT llCustCatPro THEN 
         DO:
            FIND FIRST CliType WHERE CliType.Brand = Syst.Var:gcBrand AND CliType.CliType = pcCliType NO-LOCK NO-ERROR.
            IF AVAIL CliType AND CliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
               fSetError('Mobile line for non-pro customer from PRO channel').
         END.
      END.
   END.
END.
   
fGetAllowedExtralineCliTypeListAndStatus(OUTPUT lcExtraLineCliTypeListAllowed, 
                                         OUTPUT liExtraLineStatus).

lcReturnStruct = add_struct(response_toplevel_id, "").
add_boolean(lcReturnStruct, 'order_allowed', llOrderAllowed).
add_string (lcReturnStruct, 'segment'      , fgetCustSegment(pcIdType, 
                                                             plSelfEmployed, 
                                                             llProChannel,
                                                             pcPersonId,
                                                             OUTPUT lccategory)).
add_int    (lcReturnStruct, 'subscription_limit', liSubLimit).   

IF plSTCMigrate THEN 
DO:
   add_boolean(lcReturnStruct,"subscription_limit_reached",FALSE).
   add_boolean(lcReturnStruct,"activation_limit_reached"  ,FALSE).
END.
ELSE
DO:
   add_boolean(lcReturnStruct,"subscription_limit_reached", (IF liSubs >= liSubLimit THEN TRUE ELSE FALSE)).
   add_boolean(lcReturnStruct,"activation_limit_reached"  , (IF liActs >= liActLimit THEN TRUE ELSE FALSE)).
END.

add_string (lcReturnStruct, 'additional_line_allowed', fGetAddLineAllowed()).
add_string (lcReturnStruct, 'extra_line_allowed'     , lcExtraLineCliTypeListAllowed).
add_int    (lcReturnStruct, 'extra_line_status'      , liExtraLineStatus).

IF NOT llOrderAllowed THEN 
DO:
   add_string(lcReturnStruct, 'reason' ,lcReason).
   add_string(lcReturnStruct, 'reasons',lcReasons).
END. 

FINALLY:
END.
