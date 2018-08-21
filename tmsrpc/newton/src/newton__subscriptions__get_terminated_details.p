/**
 * Get terminated subscription details
 *
 * @input       int;mandatory;subscription id
                boolean;mandatory;admin user (admin has access to terminated subs older than 6 months)
 * @output      cli;string;msisdn
                activation_time;DateTime;
                icc;string;id of the SIM card
                subscription_type_id;string;
                payment_method;string;"prepaid" or "postpaid"
                custnum;int;
                segmentation_code;string;segmentation code
                pin1;string;these four only if IMSI record exists
                pin2;string;
                puk1;string;
                puk2;string;
                billing_permission;int;billing permission status (0 = ok, 1=suspended for determined period, 2=prohibited totally)
                id_code;string;subscription pin code
                number_type;string;new/mnp
                termination_date;date;date of termination
                termination_reason;string;numerical value 1,2,.. (empty if unknown)
                termination_reason_description;string;
                orders;array of order structs;
                sub_terminals;array of terminal structs;
                sub_laptops;array of laptop structs;
                memo_counts;struct;
                satisfaction_value;string;satisfaction value 
                mnp_available;int;0 = not found, 1 = found inactive, 2 = found active
                data_bundle_id;string;data bundle id for CONTRD/CONTF
 * @order       id;int;order id
                date;date;purchase date
                contract_id;int;web order id
 * @terminal    billing_item_id;string;terminal productcode
                imei;string;terminal IMEI
                sub_terminal_id;int;unique subscription terminal id
                delivered;datetime;order creation stamp
 * @laptop      laptop;string;laptop billing item
                ordered;datetime;order creation stamp
                serial_number;string;laptop serial number
 * @memo_counts mobsub;int;0 = no memos, 1 = at least one memo
                customer;int;0 = no memos, 1 = at least one memo
                invoice;int;0 = no memos, 1 = at least one memo
                service;int;0 = no memos, 1 = at least one memo
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:katun = "Newton".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/flimitreq.i}
{Mm/fbundle.i}
{newton/src/get_memos.i}
{Func/msisdn_prefix.i}

DEF VAR plAdmin AS LOG NO-UNDO.

FUNCTION fIsViewableTermMobsub RETURNS LOGICAL
   (iiMsSeq AS INTEGER):

   DEF VAR ldaDate AS DATE NO-UNDO. 
   DEF VAR liTime AS INT NO-UNDO. 
   DEF BUFFER MsOwner FOR MsOwner.
   
   FIND FIRST Msowner WHERE 
              Msowner.msseq EQ iiMsSeq AND
              Msowner.tsend LT Func.Common:mMakeTS()
   NO-LOCK USE-INDEX MsSeq NO-ERROR.
   IF NOT AVAIL Msowner THEN RETURN FALSE.
   
   Func.Common:mSplitTS(msowner.tsend, output ldaDate, output liTime).
   IF TODAY - ldaDate > 180 AND NOT plAdmin THEN RETURN FALSE.

   RETURN TRUE.

END FUNCTION. 

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.
/* Output parameters */
DEF VAR resp_struct      AS CHAR NO-UNDO.
DEF VAR memo_struct      AS CHAR NO-UNDO.
DEF VAR term_array       AS CHAR NO-UNDO.
DEF VAR laptop_array     AS CHAR NO-UNDO.
DEF VAR term_struct      AS CHAR NO-UNDO.
DEF VAR order_array      AS CHAR NO-UNDO.
DEF VAR order_struct     AS CHAR NO-UNDO.
/* Local variables */
DEF VAR ldaTermDate      AS DATE NO-UNDO. 
DEF VAR liTermTime       AS INT  NO-UNDO. 
DEF VAR liMNPOutExists   AS INT  NO-UNDO.
DEF VAR lcDataBundle     AS CHAR NO-UNDO.
DEF VAR lcSegment        AS CHAR NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
DEF VAR llYoigoTenant    AS LOG NO-UNDO INIT FALSE.
DEF VAR llMasmovilTenant AS LOG NO-UNDO INIT FALSE.

IF validate_request(param_toplevel_id, "int,boolean") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id, "0").
plAdmin = get_bool(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO Mobile TermMobSub MsSeq piMsSeq}

ASSIGN
   llYoigoTenant    = (IF vcTenant = {&TENANT_YOIGO}    THEN TRUE ELSE FALSE)  
   llMasmovilTenant = (IF vcTenant = {&TENANT_MASMOVIL} THEN TRUE ELSE FALSE).

IF NOT fIsViewableTermMobsub(TermMobSub.MsSeq) THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

resp_struct = add_struct(response_toplevel_id, "").

add_string(resp_struct, "cli", TermMobSub.cli).
add_timestamp(resp_struct, "activation_time", TermMobSub.ActivationTS).
add_string(resp_struct, "icc", TermMobSub.ICC).
add_string(resp_struct, "subscription_type_id", TermMobSub.CLIType).
add_string(resp_struct, "payment_method", 
   (IF TermMobSub.PayType THEN 'prepaid' ELSE 'postpaid')).
add_int(resp_struct, "custnum", TermMobSub.custnum).

FIND FIRST Segmentation NO-LOCK WHERE
           Segmentation.MsSeq = piMsSeq NO-ERROR.
IF NOT AVAILABLE Segmentation THEN lcSegment = "".
ELSE lcSegment = Segmentation.SegmentCode.
add_string(resp_struct, "segmentation_code", lcSegment).

FIND IMSI NO-LOCK WHERE
     IMSI.ICC = TermMobSub.ICC NO-ERROR.
IF AVAILABLE IMSI THEN DO:
    add_string(resp_struct, "pin1", IMSI.PIN1).
    add_string(resp_struct, "pin2", IMSI.PIN2).
    add_string(resp_struct, "puk1", IMSI.PUK1).
    add_string(resp_struct, "puk2", IMSI.PUK2).
END.
   
fGetLimit(TermMobSub.InvCust, TermMobSub.MsSeq, {&LIMIT_TYPE_BILLPERM}, 0, 0, TODAY).
add_int(resp_struct,"billing_permission",
(IF AVAIL Limit THEN INT(Limit.LimitAmt) ELSE 0)).

add_string(resp_struct, "id_code", TermMobSub.IDCode).

lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

IF LOOKUP(TermMobsub.CLIType,lcBundleCLITypes) > 0 THEN DO:
   IF TermMobsub.TariffBundle > "" THEN
      lcDataBundle = TermMobsub.TariffBundle.
   ELSE
      lcDataBundle = fGetTerminatedSpecificBundle(TermMobsub.MsSeq,
                                                  Func.Common:mMakeTS(),
                                                  TermMobsub.CliType).
END. /* IF LOOKUP(TermMobsub.CLIType,lcBundleCLITypes) > 0 THEN DO: */

add_string(resp_struct,"data_bundle_id",lcDataBundle).

/* Check if subscription has MNP OUT processes */
liMNPOutExists = 0.
MNPSUB_LOOP:
FOR EACH MNPSub NO-LOCK WHERE
   MNPSub.MsSeq = TermMobSub.MsSeq,
   FIRST MNPProcess WHERE 
         MNPProcess.MNPSeq = MNPSub.MNPSeq AND
         MNPProcess.MNPType = {&MNP_TYPE_OUT} NO-LOCK:
   IF LOOKUP(STRING(MNPProcess.StatusCode),"2,5") > 0 THEN DO:
      liMNPOutExists = 2.
      LEAVE MNPSUB_LOOP.
   END.
   liMNPOutExists = 1.
END.
add_int(resp_struct, "mnp_available", liMNPOutExists).

/* NEW or MNP number */
FIND FIRST Order NO-LOCK WHERE 
           Order.MsSeq = TermMobSub.MsSeq AND
           Order.StatusCode EQ "6" AND
           Order.OrderType < 2 NO-ERROR.
IF AVAIL Order THEN DO: 
   add_string(resp_struct, "number_type",
      STRING(Order.MNPStatus EQ 0, "new/mnp")).
END.
ELSE DO: 
   add_string(resp_struct, "number_type",
      STRING(((fISYoigoCLI(TermMobSub.CLI) AND llYoigoTenant) OR 
             (fIsMasmovilCLI(TermMobSub.CLI) AND llMasmovilTenant)), "new/mnp")).
END.

FIND FIRST Msowner WHERE 
           Msowner.msseq EQ TermMobsub.MSseq AND
           Msowner.tsend LT Func.Common:mMakeTS()
NO-LOCK USE-INDEX MsSeq NO-ERROR.
IF AVAIL msowner THEN DO:
   Func.Common:mSplitTS(msowner.tsend, OUTPUT ldaTermDate, OUTPUT liTermTime).
   add_datetime(resp_struct, "termination_date", ldaTermDate).
END.

FIND MsRequest WHERE
     MsRequest.MsSeq = TermMobsub.MsSeq AND
     MsRequest.ReqType = 18 AND
     MsRequest.ReqStatus = 2 NO-LOCK NO-ERROR.
add_string(resp_struct, "termination_reason", (if avail msrequest THEN msrequest.reqcparam3 ELSE "")).
IF AVAIL msrequest THEN DO:
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "MsRequest" AND
              TMSCodes.FieldName = "TermReason" AND
              TMSCodes.CodeValue = msrequest.reqcparam3 NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN 
      add_string(resp_struct,
                 "termination_reason_description",
                 TMSCodes.CodeName).
END.

/* orders */
order_array = add_array(resp_struct,"orders").
FOR EACH Order NO-LOCK WHERE
         Order.MsSeq = TermMobSub.MsSeq AND
         Order.OrderType <= {&ORDER_TYPE_STC}:

   order_struct = add_struct(order_array,"").
   add_int(order_struct,"id",Order.OrderId).
   add_timestamp(order_struct,"date",Order.CrStamp).
   add_string(order_struct,"contract_id",Order.ContractID).
   CASE Order.OrderType :
        WHEN 0  THEN add_string(order_struct,"type","new").
        WHEN 1  OR WHEN 3 THEN add_string(order_struct,"type","mnp").
        WHEN 2  THEN add_string(order_struct,"type","renewal").
        WHEN 4  THEN add_string(order_struct,"type","stc").
   END.
END.

/* subscription terminals  */

term_array = add_array(resp_struct,"sub_terminals").
FOR EACH SubsTerminal NO-LOCK WHERE 
         SubsTerminal.MsSeq = TermMobSub.MsSeq AND
         SubsTerminal.TerminalType = {&TERMINAL_TYPE_PHONE}
         USE-INDEX MsSeq BREAK BY SubsTerminal.MsSeq:

   term_struct = add_struct(term_array,"").
   add_string(term_struct,"imei", SubsTerminal.IMEI).
   add_string(term_struct,"billing_item_id", SubsTerminal.BillCode). 
   add_int(term_struct,"sub_terminal_id", SubsTerminal.TerminalId). 
   add_timestamp(term_struct,"delivered",SubsTerminal.PurchaseTS).
END.

laptop_array = add_array(resp_struct,"sub_laptops").
FOR EACH SubsTerminal NO-LOCK WHERE 
         SubsTerminal.MsSeq = TermMobSub.MsSeq AND
         SubsTerminal.TerminalType = {&TERMINAL_TYPE_LAPTOP}
         USE-INDEX MsSeq:
   term_struct = add_struct(laptop_array,"").
   add_string(term_struct,"serial_number", SubsTerminal.IMEI).
   add_string(term_struct,"laptop", SubsTerminal.BillCode). 
   add_timestamp(term_struct,"ordered",SubsTerminal.PurchaseTS).
END.

/* Check if subscription has any memos of specified type */
memo_struct = add_struct(resp_struct, "memo_counts").
add_int(memo_struct,"mobsub",  INT(fMemoCount("mobsub",TermMobSub.Msseq,True))).
add_int(memo_struct,"customer",INT(fMemoCount("customer",TermMobSub.Custnum,True))).
add_int(memo_struct,"invoice", INT(fMemoCount("invoice",TermMobSub.Custnum,True))).
add_int(memo_struct,"service", INT(fMemoCount("service",TermMobSub.Msseq,True))).

/* satisfaction value */
FIND FIRST PIndicator  WHERE
           PIndicator.Brand = Syst.Var:gcBrand AND
           PIndicator.HostTable = "MobSub" AND
           PIndicator.KeyValue = STRING(TermMobSub.MsSeq) AND
           PIndicator.IndicatorType = {&P_INDICATOR_TYPE_SATISFACTION_VALUE}  
           USE-INDEX HostTable NO-LOCK NO-ERROR. 
IF AVAIL PIndicator THEN 
    add_string(resp_struct,"satisfaction_value",PIndicator.IndicatorValue) .

FINALLY:
   END.
