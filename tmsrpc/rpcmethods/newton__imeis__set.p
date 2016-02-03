/*
 * Change IMEI (IMEI change in POS Tool. YOT-314, YOT-785, YDR-122, YDR-328, YOT-1682)
 *
 * @input  order_id;int;mandatory;order id
           imei;string;mandatory;new imei
           offer_id;string;mandatory;offer id
           username;string;mandatory;vista user
           update_imei_only;string;optional;if true then only imei is changed (offer/contracts will not be updated)
 * @output boolean;true
 */

{xmlrpc/xmlrpc_access.i}

DEF VAR pcStruct AS CHARACTER NO-UNDO. 

DEF BUFFER bOfferItem FOR OfferItem.
DEF BUFFER bOfferItemTerminal FOR OfferItem.

DEF VAR pcUserName AS CHARACTER NO-UNDO. 
DEF VAR piOrderId AS INTEGER NO-UNDO. 
DEF VAR pcIMEI AS CHAR NO-UNDO. 
DEF VAR pcOfferId AS CHAR NO-UNDO.
DEF VAR pcContractID AS CHAR NO-UNDO.
DEF VAR pcChannel AS CHAR NO-UNDO.
DEF VAR liTermOfferItemID AS INTEGER NO-UNDO.
DEF VAR lcCurrentContract AS CHARACTER NO-UNDO.
DEF VAR ldaCurrentContractBegin AS DATE NO-UNDO.
DEF VAR lcError AS CHARACTER NO-UNDO. 
DEF VAR liRequest AS INTEGER NO-UNDO. 
DEF VAR liTermRequest AS INTEGER NO-UNDO. 
DEF VAR i AS INTEGER NO-UNDO. 
DEF VAR lcOldIMEI AS CHARACTER NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO. 
DEF VAR llUpdateImeiOnly AS LOG NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.
 
lcStruct = validate_request(pcStruct,"order_id!,imei!,offer_id!,username!,update_imei_only,contract_id,pc_channel").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   piOrderId = get_pos_int(pcStruct,"order_id")
   pcIMEI = get_string(pcStruct,"imei")
   pcOfferId = get_string(pcStruct,"offer_id")
   pcUserName = get_string(pcStruct,"username")
   llUpdateImeiOnly = get_bool(pcStruct,"update_imei_only") WHEN
      LOOKUP("update_imei_only",lcStruct) > 0
    pcContractID = get_string(pcStruct,"contract_id") 
      WHEN LOOKUP("contract_id", lcstruct) > 0
   pcChannel = get_string(pcStruct,"channel")
               WHEN LOOKUP("channel", lcstruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

{Syst/commpaa.i}
katun = "VISTA_" + pcUserName.
gcbrand = "1".
{Syst/tmsconst.i}
&GLOBAL-DEFINE STAR_EVENT_USER katun 
{Syst/eventval.i}
{lib/eventlog.i}
{Func/order.i}
{Func/fmakemsreq.i}
{Func/msreqfunc.i}
{Func/fpcmaintreq.i}

/* check order exist */
FIND Order NO-LOCK WHERE
     Order.Brand = gcBrand AND
     Order.OrderId = piOrderId NO-ERROR.
IF NOT AVAIL Order THEN
   RETURN appl_err(SUBST("Unknown Order id &1",STRING(piOrderId))).

IF INDEX(Order.OrderChannel,"pos") = 0 THEN
   RETURN appl_err("Only POS order channels are supported").

IF LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES} + ",73") > 0 THEN
   RETURN appl_err("Order has wrong status").

IF NOT llUpdateImeiOnly THEN DO:

FIND Offer WHERE 
     Offer.Brand = gcBrand AND 
     Offer.Offer = pcOfferId NO-LOCK NO-ERROR.
IF NOT AVAIL Offer THEN
   RETURN appl_err("Offer " + pcOfferId + " is not defined").

/* payterm must match with original and new order YDR-328 */
FIND FIRST OfferItem WHERE
           OfferItem.Brand = gcBrand AND
           OfferItem.Offer = Offer.Offer AND
           OfferItem.ItemType = "PerContract" AND
           OfferItem.ItemKey BEGINS "PAYTERM" AND
           OfferItem.BeginStamp <= fMakeTS() AND
           OfferItem.EndStamp >= fMakeTS() NO-LOCK NO-ERROR.

FIND FIRST bOfferItem WHERE
           bOfferItem.Brand = gcBrand AND
           bOfferItem.Offer = Order.Offer AND
           bOfferItem.ItemType = "PerContract" AND
           bOfferItem.ItemKey BEGINS "PAYTERM" AND
           bOfferItem.BeginStamp <= Order.CrStamp AND
           bOfferItem.EndStamp >= Order.CrStamp NO-LOCK NO-ERROR.

IF AVAIL(OfferItem) NE AVAIL(bOfferItem) THEN
   RETURN appl_err("Payterm type does not match").

IF AVAIL(OfferItem) AND AVAIL(bOfferItem) THEN DO:
    IF OfferItem.ItemKey NE bOfferItem.ItemKey OR
       OfferItem.Amount  NE bOfferItem.Amount THEN
       RETURN appl_err("Payterm type does not match").
END. /* IF AVAIL(OfferItem) AND AVAIL(bOfferItem) THEN DO: */

/* If IMEI is given then also offer must include some terminal */
IF pcIMEI > "" THEN DO:
   
   liTermOfferItemID = fGetTerminalOfferItemId(Offer.Offer, 
                        {&BITEM_GRP_TERMINAL}, fMakeTS()).
   IF liTermOfferItemID = 0 THEN
      RETURN appl_err("Terminal not found from offer " + Offer.Offer).
   
   FIND bOfferItemTerminal WHERE
        bOfferItemTerminal.OfferItemId = liTermOfferItemID NO-LOCK NO-ERROR.
   IF NOT AVAIL bOfferItemTerminal THEN
      RETURN appl_err("Terminal not found from offer " + Offer.Offer).
END.
   
/* check if new offer contains terminal contract */
i = 0.
RELEASE OfferItem.
FOR EACH bOfferItem NO-LOCK WHERE
         bOfferItem.Brand = gcBrand AND
         bOfferItem.Offer = Offer.Offer AND
         bOfferItem.ItemType = "PerContract" AND
         bOfferItem.EndStamp >= fMakeTS() AND
         bOfferItem.BeginStamp <= fMakeTS() USE-INDEX ItemType,
   FIRST DayCampaign NO-LOCK WHERE
         DayCampaign.Brand = gcBrand AND
         DayCampaign.DcEvent = bOfferItem.ItemKey AND
         DayCampaign.DcType = {&DCTYPE_DISCOUNT}:

   i = i + 1.
   IF i > 1 THEN DO:
      RETURN appl_err("Offer includes more than one periodical contract").
   END.
   
   FIND OfferItem WHERE
        ROWID(OfferItem) = ROWID(bOfferItem) NO-LOCK.
END.

IF AVAIL OfferItem AND pcIMEI EQ "" THEN
   RETURN appl_err("Given IMEI is empty but new offer contains contract").

END. /* IF NOT llUpdateImeiOnly THEN DO: */
   
CONTRACT_TRANSACTION:
DO TRANS:
      
IF Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} THEN DO:
   
   FIND MobSub NO-LOCK WHERE
        MobSub.MsSeq = Order.MsSeq NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN appl_err("Subcription not found").
   
   FIND SubsTerminal EXCLUSIVE-LOCK WHERE
        SubsTerminal.Brand = gcBrand AND
        SubsTerminal.OrderId = Order.OrderId AND
        SubsTerminal.TerminalType = {&TERMINAL_TYPE_PHONE} NO-ERROR.

   IF AMBIGUOUS(SubsTerminal) THEN
      RETURN appl_err("More than one subscription terminal found for order " +
                      STRING(Order.Orderid)).

   /* If not available then create new terminal (if given IMEI is not empty) */
   IF NOT AVAIL SubsTerminal THEN DO:

      IF pcIMEI = "" THEN RETURN appl_err("IMEI not changed").
   
      DEF VAR liTerminalID AS INT NO-UNDO.
      
      FIND LAST SubsTerminal USE-INDEX TerminalID NO-LOCK NO-ERROR.
      IF AVAILABLE SubsTerminal THEN liTerminalID = SubsTerminal.TerminalID + 1.
      ELSE liTerminalID = 1.
      
      CREATE SubsTerminal.
      
      REPEAT:
         
         SubsTerminal.TerminalID = liTerminalID NO-ERROR.
         
         VALIDATE SubsTerminal NO-ERROR.

         IF ERROR-STATUS:ERROR OR SubsTerminal.TerminalID = 0 THEN DO:
            liTerminalID = liTerminalID + 1.
            NEXT.
         END.
         ELSE LEAVE.
      END.

      ASSIGN
         SubsTerminal.Brand = gcBrand
         SubsTerminal.OrderId = Order.OrderId
         SubsTerminal.IMEI = pcIMEI
         SubsTerminal.MsSeq = MobSub.MsSeq
         SubsTerminal.PurchaseTS = Order.CrStamp
         SubsTerminal.TerminalType = {&TERMINAL_TYPE_PHONE}
         SubsTerminal.BillCode = bOfferItemTerminal.ItemKey WHEN
            NOT llUpdateImeiOnly.
   END.
   ELSE IF SubsTerminal.IMEI EQ pcIMEI THEN DO:
      UNDO CONTRACT_TRANSACTION, RETURN appl_err("IMEI is same with old one").
   END.

   IF NOT llUpdateImeiOnly THEN DO:
   i = 0.
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq = MobSub.MsSeq AND
            DCCLI.ValidFrom <= TODAY AND
            DCCLI.ValidTo >= TODAY,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} 
      NO-LOCK BY DCCLI.ValidFrom DESC:

      i = i + 1.

      ASSIGN
         lcCurrentContract = DCCLI.DCEvent
         ldaCurrentContractBegin = DCCLI.ValidFrom.

      IF i > 1 THEN UNDO CONTRACT_TRANSACTION,
         RETURN appl_err("More than one active terminal contracts").
   END.
  
   /* remove active contract if
      empty imei is given or new contract differs from old one */
   IF lcCurrentContract NE "" AND
      (pcIMEI = "" OR
      (AVAIL OfferItem AND 
             OfferItem.ItemKey NE lcCurrentContract
      )) THEN DO:
      
      liTermRequest = fPCMaintenanceRequest(MobSub.MsSeq,
                                        lcCurrentContract,
                                        "ValidTo",
                                        STRING(ldaCurrentContractBegin - 1),
                                        ?,
                                        FALSE,
                                        {&REQUEST_SOURCE_NEWTON},
                                        "",
                                        0, /* main request id  */
                                        FALSE, /* is mandatory subrequest */ 
                                        OUTPUT lcError).

      IF liTermRequest = 0 THEN DO:
         IF llDoEvent THEN fCleanEventObjects().
         UNDO CONTRACT_TRANSACTION, RETURN appl_err(lcError).
      END.
   END.
      
   /* create new contract if
      imei is given and new offer includes terminal
      and current contract does not exist or it differs from old one
   */
   IF pcIMEI > "" AND AVAIL OfferItem AND
      (lcCurrentContract EQ "" OR
      (OfferItem.ItemKey NE lcCurrentContract)) THEN DO:
      
      liRequest = fPCActionRequest(MobSub.MsSeq,
                                   OfferItem.ItemKey,
                                   "recreate",
                                   Order.CrStamp,
                                   FALSE, /* no fees */
                                   {&REQUEST_SOURCE_NEWTON},
                                   "",
                                   0,
                                   FALSE,
                                   "",
                                   0,
                                   0,
                                   OUTPUT lcError). 

      IF liRequest = 0 THEN
         UNDO CONTRACT_TRANSACTION,
         RETURN appl_err("Contract request creation failed").
   END.
   END. /* IF NOT llUpdateImeiOnly THEN DO: */

   IF NOT NEW SubsTerminal THEN DO:
      
      lcOldIMEI = SubsTerminal.IMEI.
         
      IF llDoEvent THEN DO:
         DEF VAR lhSubsTerminal AS HANDLE NO-UNDO.
         lhSubsTerminal = BUFFER SubsTerminal:HANDLE.
         RUN StarEventInitialize(lhSubsTerminal).
      END.
      
      IF pcIMEI > "" THEN DO:
         
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSubsTerminal).

         ASSIGN 
            SubsTerminal.BillCode = bOfferItemTerminal.ItemKey WHEN
               NOT llUpdateImeiOnly 
            Substerminal.PerContractID = 0 WHEN liTermRequest > 0
            Substerminal.IMEI = pcIMEI.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSubsTerminal).

      END.
      ELSE DO:
         IF llDoEvent THEN RUN StarEventMakeDeleteEvent( lhSubsTerminal ).
         DELETE Substerminal.
      END.
   END.

END.
ELSE DO:

   FIND OrderAccessory EXCLUSIVE-LOCK WHERE
        OrderAccessory.Brand = gcBrand AND
        OrderAccessory.OrderId = Order.OrderId AND
        OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-ERROR.

   IF AMBIGUOUS OrderAccessory THEN
      RETURN appl_err("Order includes more than one terminal").
   ELSE IF NOT AVAIL OrderAccessory AND pcIMEI EQ "" THEN
      RETURN appl_err("Order doesn't contain any terminal, cannot remove IMEI").
      
   IF llDoEvent THEN DO:
      DEF VAR lhOrderAccessory AS HANDLE NO-UNDO.
      lhOrderAccessory = BUFFER OrderAccessory:HANDLE.
      RUN StarEventInitialize(lhOrderAccessory).
   END.
   
   IF pcIMEI EQ "" THEN DO:
      IF llDoEvent THEN RUN StarEventMakeDeleteEvent( lhOrderAccessory ).
      DELETE OrderAccessory.
   END.
   ELSE DO:
      
      IF AVAIL OrderAccessory THEN DO:

         IF OrderAccessory.IMEI EQ pcIMEI THEN
            RETURN appl_err("IMEI is same, not changed").

         lcOldIMEI = OrderAccessory.IMEI.
      
         FIND CURRENT OrderAccessory EXCLUSIVE-LOCK.

         IF llDoEvent THEN DO:
            RUN StarEventSetOldBuffer(lhOrderAccessory).
         END.
      END.
      ELSE DO:
         CREATE OrderAccessory.
         ASSIGN
            OrderAccessory.Brand = gcBrand
            OrderAccessory.OrderId = Order.OrderID.
            OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE}.
      END.
          
      ASSIGN 
         OrderAccessory.ProductCode = bOfferItemTerminal.ItemKey WHEN
            NOT llUpdateImeiOnly  
         OrderAccessory.Amount = bOfferItemTerminal.Amount WHEN
            NOT llUpdateImeiOnly 
         OrderAccessory.IMEI = pcIMEI.

      IF llDoEvent THEN DO:
         IF NEW OrderAccessory THEN RUN StarEventMakeCreateEvent(lhOrderAccessory).
         ELSE RUN StarEventMakeModifyEvent(lhOrderAccessory). 
      END.

      RELEASE OrderAccessory.
   END.
   
   IF NOT llUpdateImeiOnly AND Order.Offer NE pcOfferId THEN DO: 

      FIND CURRENT Order EXCLUSIVE-LOCK.
      
      IF llDoEvent THEN DO:
         DEF VAR lhOrder AS HANDLE NO-UNDO.
         lhOrder = BUFFER Order:HANDLE.
         RUN StarEventInitialize(lhOrder).
         RUN StarEventSetOldBuffer(lhOrder).
      END.
      
      Order.Offer = pcOfferId.
   
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).
  
      FIND CURRENT Order NO-LOCK.
   END.
END.

fCreateRequest({&REQTYPE_IMEI_CHANGE}, /* heat balance query request */
               0 , /* chgstamp */
               "", /* creator */
               FALSE, /* create fees */
               FALSE). /* send sms */

/*ContractID can be given without checking because
this RPC is used only in POS. */
ASSIGN
   bCreaReq.msseq = Order.msseq
   bCreaReq.custnum = Order.custnum
   bCreaReq.CLI = Order.cli
   bCreaReq.reqcparam1 = lcOldIMEI
   bCreaReq.reqcparam2 = pcIMEI
   bCreaReq.reqcparam3 = pcOfferId
   bCreaReq.reqcparam6 = Order.Contractid /*YTS-7939, take from order*/
   bCreaReq.reqiparam1 = Order.OrderId
   bCreaReq.ReqSource  = {&REQUEST_SOURCE_NEWTON}.

FIND MSRequest WHERE
     ROWID(MsRequest) EQ ROWID(bCreaReq) NO-LOCK.

fReqStatus(2,"").

END. /* CONTRACT_TRANSACTION */

IF lcError NE "" THEN RETURN appl_err(lcError).
   
add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
