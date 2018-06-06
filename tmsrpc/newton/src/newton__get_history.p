/**
 * RPC for getting completed and pending requests for a subscription.
 *
 * @input: msseq;int;mandatory;Mobile subscription identifier
           last;int;mandatory;Sequence value of the last known memo
 * @output requests;array;containing one history request structure
 * @struct change_type;string;
           old_value;string;
           new_value;string;
           change_time;datetime;
           admin_cancel;boolean;true is only admin user can cancel request
           request_id;int;
           status;int;
           actor;string;the person who requested the change
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.
DEF VAR piLatest AS INT NO-UNDO.
/* Output parameters */
DEF VAR top_array AS CHAR NO-UNDO.
DEF VAR hist_struct AS CHAR NO-UNDO.
/* Local variables */
DEF BUFFER SecondaryRequest FOR MsRequest.
DEF VAR gcSecondaryCache AS CHAR NO-UNDO.
DEF VAR lcServiceVal AS CHAR NO-UNDO.
DEF VAR liMsSeq AS INT NO-UNDO. 
DEF VAR liAgrCust AS INT NO-UNDO. 
DEF VAR lcOldValue AS CHAR NO-UNDO.
DEF VAR lcNewValue AS CHAR NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
DEF VAR lcTariffBundle   AS CHAR NO-UNDO.
DEF VAR lcCLIType AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "int,int") EQ ? THEN RETURN.
piLatest = get_int(param_toplevel_id, "1").
piMsSeq = get_pos_int(param_toplevel_id, "0").
IF piMsSeq = ? OR piLatest = ? THEN RETURN.

FIND MobSub NO-LOCK WHERE
     Mobsub.msseq = piMsSeq AND
     Mobsub.Brand = Syst.Var:gcBrand NO-ERROR.
IF NOT AVAILABLE mobsub THEN DO:
   FIND TermMobsub NO-LOCK WHERE
        TermMobsub.msseq = piMsSeq AND
        TermMobsub.brand = Syst.Var:gcBrand NO-ERROR.
   IF NOT AVAIL TermMobsub THEN
      RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).
   ASSIGN
      liMsSeq = TermMobsub.MsSeq
      liAgrCust = TermMobsub.AgrCust
      lcTariffBundle = TermMobsub.TariffBundle.
END.
ELSE ASSIGN
   liMsSeq = MobSub.MsSeq
   liAgrCust = MobSub.AgrCust
   lcTariffBundle = MobSub.TariffBundle.

lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

top_array = add_array(response_toplevel_id, "").

/* Requests associated with the address change */
FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand = "1" AND
         MsRequest.ReqType = 6 AND
         MsRequest.CustNum = liagrcust AND
         (MsRequest.ReqStatus = 2 OR
          MsRequest.ReqStatus = 0) AND
         MsRequest.MsRequest >= piLatest
    BY MsRequest.DoneStamp:

    hist_struct = add_struct(top_array, "").

    add_string(hist_struct, "change_type", 'address').
    add_string(hist_struct, "new_value",
               MsRequest.ReqCParam1 + ";" +
               MsRequest.ReqCParam2 + ";" +
               MsRequest.ReqCParam3 + ";" +
               MsRequest.ReqCParam4 + ";" +
               STRING(MsRequest.ReqiParam1) + ";" +
               STRING(MsRequest.ReqiParam2) + ";" +
               STRING(MsRequest.ReqiParam3) + ";" +
               STRING(MsRequest.ReqiParam4)).

    add_int(hist_struct, "request_id", MsRequest.MsRequest).
    add_int(hist_struct, "status", MsRequest.ReqStatus).
    add_string(hist_struct, "actor", MsRequest.UserCode).
    add_boolean(hist_struct, "admin_cancel", false).
    IF MsRequest.DoneStamp NE ? AND MsRequest.DoneStamp NE 0 THEN
        add_timestamp(hist_struct, "change_time", MsRequest.DoneStamp).
    ELSE
        add_timestamp(hist_struct, "change_time", MsRequest.ActStamp).
END.

/* Requests by msseq */
FOR EACH MsRequest NO-LOCK USE-INDEX MsSeq WHERE
         MsRequest.MsSeq   = limsseq AND
         (MsRequest.ReqType = 0 OR
          MsRequest.ReqType = 1 OR
          MsRequest.ReqType = 10 OR 
          MsRequest.ReqType = 15 OR
          MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} OR
          MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_REACTIVATION}) AND
          MsRequest.MsRequest >= piLatest
    BY MsRequest.DoneStamp:
    
    CASE MsRequest.ReqType:
      WHEN 0 THEN
         IF LOOKUP(STRING(MsRequest.ReqStatus),"0,2,8,19") = 0 THEN NEXT.
      WHEN 10 THEN 
         IF LOOKUP(STRING(MsRequest.ReqStatus),"0,2,8") = 0 THEN NEXT.
      WHEN 15 THEN DO:
         IF LOOKUP(STRING(MsRequest.ReqStatus),"0,2,19,20") = 0 THEN NEXT.
      END.
      WHEN {&REQTYPE_BUNDLE_CHANGE} THEN DO:
         IF LOOKUP(STRING(MsRequest.ReqStatus),"0,2,8") = 0 THEN NEXT.

         /* Display event only for IPL/FLAT Tariffs */
         IF INDEX(MsRequest.ReqCParam1,"CONTD") = 0 AND
            INDEX(MsRequest.ReqCParam1,"CONTF") = 0 AND
            INDEX(MsRequest.ReqCParam1,"CONTS") = 0 THEN NEXT.
      END. /* WHEN {&REQTYPE_BUNDLE_CHANGE} THEN DO: */
      WHEN {&REQTYPE_SUBSCRIPTION_REACTIVATION} THEN .
      OTHERWISE
         IF LOOKUP(STRING(MsRequest.ReqStatus),"0,2") = 0 THEN NEXT.
    END.

    IF ( MsRequest.ReqType = 1 AND 
      LOOKUP(MsRequest.ReqCParam1,'SMS,VMS',',') = 0 
      ) THEN
        NEXT.
    IF LOOKUP(STRING(MsRequest.MsRequest), gcSecondaryCache) > 0 THEN NEXT.

    hist_struct = add_struct(top_array, "").
    IF MsRequest.ReqType >= 0  AND
       MsRequest.ReqType <= 15 AND
       MsRequest.ReqType <> 1  THEN
      add_string(hist_struct, "change_type", ENTRY(MsRequest.ReqType + 1,
                                        "subscription_type,sms_pack,?,user," +
                                        "payer,?,?,?,?,?,acc,?,?,?,?,icc")).
    add_int(hist_struct, "request_id", MsRequest.MsRequest).
    add_int(hist_struct, "status", MsRequest.ReqStatus).
    add_string(hist_struct, "actor", MsRequest.UserCode).
    
    IF MsRequest.DoneStamp NE ? AND MsRequest.DoneStamp NE 0 THEN
        add_timestamp(hist_struct, "change_time", MsRequest.DoneStamp).
    ELSE IF MsRequest.ReqType = 10 AND
        MsRequest.ReqDparam1 > MsRequest.ActStamp THEN 
        add_timestamp(hist_struct, "change_time", MsRequest.ReqDParam1).
    ELSE add_timestamp(hist_struct, "change_time", MsRequest.ActStamp).
   
    /* ReqIParam2 is order id from renewal pos order */
    IF MsRequest.ReqType = 0 THEN 
       add_boolean(hist_struct, "admin_cancel", MsRequest.ReqIParam2 > 0).
    ELSE add_boolean(hist_struct, "admin_cancel", FALSE).

    /* ReqType = 1: sms_pack */
    IF MsRequest.ReqType EQ 1 THEN DO:
        
        IF LOOKUP(MsRequest.ReqCParam1,'SMS') NE 0 THEN DO: 
           add_string(hist_struct, "change_type", "sms_pack"). 
           IF MsRequest.ReqIParam1 EQ 1 THEN DO:
               add_string(hist_struct, "new_value", MsRequest.ReqCParam1).
               IF MsRequest.ReqCParam4 NE "" THEN DO:
                   FIND FIRST SecondaryRequest
                   WHERE SecondaryRequest.MsRequest EQ
                           INT(ENTRY(1, MsRequest.ReqCParam4)) NO-ERROR.
                   IF AVAILABLE SecondaryRequest THEN DO:
                       gcSecondaryCache = gcSecondaryCache + "," + 
                                           STRING(SecondaryRequest.MsRequest).
                       add_string(hist_struct, "old_value",
                                           SecondaryRequest.ReqCParam1).
                   END.
               END.
           END. ELSE DO:
               add_string(hist_struct, "old_value", MsRequest.ReqCParam1).
               add_string(hist_struct, "new_value", "off").
           END.
        END.
        ELSE DO: 
           add_string(hist_struct, "change_type", "vms"). 
           lcServiceVal = (IF MsRequest.ReqCParam2 NE "" 
                           THEN MsRequest.ReqCParam2
                           ELSE STRING(MsRequest.ReqIParam1)).
           add_string(hist_struct, "new_value", 
               (IF lcServiceVal EQ "1" THEN "on" 
                  ELSE IF lcServiceVal EQ "0" THEN "off"
                  ELSE lcServiceVal)).
        END.
    END. 
    /* ICC change */
    ELSE IF MsRequest.ReqType EQ 15 THEN DO:
       /* currently getting old icc value is not possible/reliable */
       add_string(hist_struct, "old_value", "").
       add_string(hist_struct, "new_value", MsRequest.ReqCParam2).    
    END. 
    /* ReqType = 77: duplicate_invoice */
    ELSE IF MsRequest.ReqType = {&REQTYPE_DUPLICATE_INVOICE} THEN DO:
        add_string(hist_struct, "new_value", MsRequest.ReqCParam1).
        add_string(hist_struct, "change_type", "duplicate_invoice"). 
    END.
    /* ReqType = 0: subscription_type */
    ELSE IF MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN DO:
        ASSIGN lcOldValue = MsRequest.ReqCParam1
               lcNewValue = MsRequest.ReqCParam2.

        IF LOOKUP(MsRequest.ReqCParam1,lcBundleCLITypes) > 0 THEN
           lcOldValue = lcOldValue + lcTariffBundle.
        IF LOOKUP(MsRequest.ReqCParam2,lcBundleCLITypes) > 0 THEN
           lcNewValue = lcNewValue + MsRequest.ReqCParam5.

        add_string(hist_struct, "old_value", lcOldValue).
        add_string(hist_struct, "new_value", lcNewValue).
    END.
    /* ReqType = 81: Bundle Type Change */
    ELSE IF MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} THEN DO:

        lcCLIType = SUBSTRING(MsRequest.ReqCParam1,1,5).

        IF LOOKUP(lcCLIType,lcBundleCLITypes) > 0 THEN DO:
           IF lcCLIType EQ "CONTD" THEN lcCLIType = "CONTRD".
           add_string(hist_struct,"old_value",lcCLIType + MsRequest.ReqCParam1).
           add_string(hist_struct,"new_value",lcCLIType + MsRequest.ReqCParam2).
        END.

        add_string(hist_struct, "change_type", "bundle_type_change").
    END. /* ELSE IF MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_REACTIVATION} */
    /* ReqType = 82: subscription reactivation */
    ELSE IF MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_REACTIVATION} THEN DO:
        add_string(hist_struct, "new_value", MsRequest.ReqCParam1).
        add_string(hist_struct, "change_type", "Subscription Reactivation").
    END. /* ELSE IF MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_REACTIVATION} */
    /* ELSE: owner, user or payer change */
    ELSE DO:
      IF MsRequest.ReqType = 10 THEN DO:
         add_string(hist_struct, "old_value", STRING(MsRequest.Custnum)).
      
      /* IF MsRequest.ReqIParam1 > 0 THEN we change to an existing customer */
         IF MsRequest.ReqIParam1 > 0 THEN
            add_string(hist_struct, "new_value", STRING(MsRequest.ReqIParam1)).
         ELSE IF MsRequest.ReqIParam4 > 0 THEN DO:
            FIND OrderCustomer NO-LOCK WHERE
                 OrderCustomer.Brand   = Syst.Var:gcBrand     AND
                 OrderCustomer.OrderID = MsRequest.ReqIParam4 AND
                 OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_ACC}
            NO-ERROR.
            IF AVAILABLE OrderCustomer THEN
               add_string(hist_struct,"new_value",
                  OrderCustomer.CustIDType + ";" +
                  OrderCustomer.CustID).
         END.
         ELSE IF NUM-ENTRIES(MsRequest.ReqCParam1,";") >= 13 THEN
            add_string(hist_struct,"new_value",
                  ENTRY(12,MsRequest.ReqCParam1,";") + ";" +
                  ENTRY(13,MsRequest.ReqCParam1,";")).
            
      END.
   END.
END.

FINALLY:
   END.
