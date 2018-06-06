/**
 * A mobsub object
 *
 * @input       msseq;int;
 * @output      cli;string;
                fixed_number;string;
                msstatus;int;(4 = active, 8 = barred)
                barring_code;string;
                activation_time;DateTime;
                pin1;string;these four only if IMSI record exists
                pin2;string;
                puk1;string;
                puk2;string;
                icc;string;id of the SIM card
                subscription_type_id;string;
                payment_method;string;"prepaid" or "postpaid"
                custnum;int;
                billing_permission;int;billing permission status (0 = ok, 1=suspended for determined period, 2=prohibited totally)
                memo_counts;struct;
                segmentation_code;string;segmentation code
                segmentation_offer;string;segmentation offer renewal
                data_bundle_id;string;data bundle id
                mandate_id;string;mandate id
                mandate_date;datetime;mandate date
                satisfaction_value;string;satisfaction value 
                permanent_contract_valid_to;datetime;
                permanent_contract_original_valid_to;datetime;
                permanent_contract_valid_from;datetime;
                fixed_permanent_contract_valid_to;datetime;optional;
                fixed_permanent_contract_valid_from;datetime;optional;
                fixed_permanent_contract_original_valid_to;datetime;optional;
                mnp_available;int;0 = not found, 1 = found inactive, 2 = found active
                multisim_type;int;optional;multisim subscription type (1=primary, 2=secondary)
                multisim_msisdn;string;optional;multisim primary/secondary msisdn
                multisim_warning_for_secondary;boolean;optional;warning flag for secondary line
                tarj7_reset_date;datetime;optional;Reset date (if past date then use red otherwise green color)
 * @memo_counts mobsub;int;0 = no memos, 1 = at least one memo
                customer;int;0 = no memos, 1 = at least one memo
                invoice;int;0 = no memos, 1 = at least one memo
                service;int;0 = no memos, 1 = at least one memo
 * @sub_terminals array of terminal structs;subscription terminals
 * @terminal    billing_item_id;string;terminal productcode
                imei;string;terminal IMEI
                sub_terminal_id;int;unique subscription terminal id
                delivered;datetime;order creation stamp
 * @sub_laptops array of laptop structs;subscription laptops
 * @laptop      laptop;string;laptop billing item
                ordered;datetime;order creation stamp
                serial_number;string;laptop serial number
 * @installments;array of installment_contract structs;
 * @installment_contract 
                pending_fee;double;how much is unpaid
                final_fee;double;residual amount final payment
                per_contract_id;int;unique contract id 
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{newton/src/get_memos.i}
{Func/msisdn_prefix.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.
/* Output parameters */
DEF VAR resp_struct    AS CHAR NO-UNDO.
DEF VAR memo_struct    AS CHAR NO-UNDO.
DEF VAR term_array     AS CHAR NO-UNDO.
DEF VAR laptop_array   AS CHAR NO-UNDO.
DEF VAR term_struct    AS CHAR NO-UNDO.
DEF VAR order_array    AS CHAR NO-UNDO.
DEF VAR order_struct   AS CHAR NO-UNDO.
DEF VAR payterm_struct AS CHAR NO-UNDO.
/* Local variables */
DEF VAR lcPriceList    AS CHAR NO-UNDO. 
DEF VAR liMNPOutExists AS INT  NO-UNDO.
DEF VAR lcOrigCLIType  AS CHAR NO-UNDO.
DEF VAR liCountMobile  AS INT  NO-UNDO.
DEF VAR liCountFixed   AS INT  NO-UNDO.
DEF VAR liMultiSimType AS INT  NO-UNDO. 
DEF VAR lcSegmentCode  AS CHAR NO-UNDO.
DEF VAR lcSegmentOffer AS CHAR NO-UNDO.
DEF VAR lcFinancedInfo AS CHAR NO-UNDO. 

DEF VAR ldeActStamp         AS DEC     NO-UNDO.
DEF VAR ldaActDate          AS DATE    NO-UNDO.
DEF VAR ldaRenewalDate      AS DATE    NO-UNDO. 
DEF VAR ldePendingFee       AS DECIMAL NO-UNDO. 
DEF VAR liTotalPeriods      AS INTEGER NO-UNDO. 
DEF VAR ldePeriodFee        AS DECIMAL NO-UNDO.
DEF VAR ldeFinalAmt         AS DECIMAL NO-UNDO.
DEF VAR liMnpStatus         AS INT     NO-UNDO.
DEF VAR liOrderId           AS INT     NO-UNDO. 
DEF VAR installment_array   AS CHAR    NO-UNDO.
DEF VAR lderesidualFee      AS DEC     NO-UNDO. 
DEF VAR liMultiSimTypeValue AS INT     NO-UNDO. 
DEF VAR lcMultiSimCLI       AS CHAR    NO-UNDO. 
DEF VAR llYoigoTenant     AS LOG    NO-UNDO INIT FALSE.
DEF VAR llMasmovilTenant  AS LOG    NO-UNDO INIT FALSE.

DEF BUFFER lbMobSub    FOR MobSub.
DEF BUFFER lbELMobSub  FOR MobSub.
DEF BUFFER lbMLMobSub  FOR MobSub.
DEF BUFFER bActRequest FOR MsRequest.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE MsOwner.MsSeq = MobSub.MsSeq NO-ERROR.
IF NOT AVAILABLE MsOwner THEN
   RETURN appl_err(SUBST("MsOwner entry &1 not found", piMsSeq)).

{Syst/commpaa.i}
Syst.Var:katun = "Newton".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/flimitreq.i}
{Func/fcustpl.i}
{Func/fixedfee.i}
{Func/fctchange.i}
{Func/multitenantfunc.i}

ASSIGN
   llYoigoTenant    = (IF vcTenant = {&TENANT_YOIGO}    THEN TRUE ELSE FALSE)  
   llMasmovilTenant = (IF vcTenant = {&TENANT_MASMOVIL} THEN TRUE ELSE FALSE).

FIND FIRST Segmentation NO-LOCK WHERE
           Segmentation.MsSeq = piMsSeq NO-ERROR.
IF AVAILABLE Segmentation THEN ASSIGN 
   lcSegmentCode = Segmentation.SegmentCode
   lcSegmentOffer = Segmentation.SegmentOffer.

resp_struct = add_struct(response_toplevel_id, "").

add_string(resp_struct, "brand",fConvertTenantToBrand(vcTenant)).
add_string(resp_struct, "cli", mobsub.cli).
IF Mobsub.fixednumber NE ? THEN
   add_string(resp_struct, "fixed_number", Mobsub.fixednumber).
ELSE
   add_string(resp_struct, "fixed_number", "").
add_int(resp_struct, "msstatus", mobsub.MsStatus).
add_string(resp_struct, "barring_code", (IF mobsub.MsStatus EQ 8 
                                         THEN mobsub.barrcode
                                         ELSE "")).
add_timestamp(resp_struct, "activation_time", mobsub.ActivationTS).
add_string(resp_struct, "icc", mobsub.ICC).
add_string(resp_struct, "subscription_type_id", mobsub.CLIType).
add_string(resp_struct, "payment_method", 
   (IF mobsub.PayType THEN 'prepaid' ELSE 'postpaid')).
add_int(resp_struct, "custnum", mobsub.custnum).
add_string(resp_struct, "segmentation_code", lcSegmentCode).
add_string(resp_struct, "segmentation_offer", lcSegmentOffer).
add_string(resp_struct, "data_bundle_id", MobSub.TariffBundle).
add_string(resp_struct, "mandate_id", MsOwner.MandateId). 
add_datetime(resp_struct, "mandate_date", MsOwner.Mandatedate). 

/* Reset Date of benefits */
IF LOOKUP(MobSub.CliType,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN
   FOR FIRST ServiceLimit NO-LOCK WHERE
             ServiceLimit.GroupCode = MobSub.CLIType:
       FIND FIRST MServiceLimit WHERE
                  MServiceLimit.MsSeq = Mobsub.MsSeq AND
                  MServiceLimit.DialType = ServiceLimit.DialType AND
                  MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
                  MServiceLimit.EndTS >= Func.Common:mMakeTS() NO-LOCK NO-ERROR.
       IF NOT AVAIL MServiceLimit THEN DO:
          FIND FIRST MServiceLimit WHERE
                     MServiceLimit.MsSeq = Mobsub.MsSeq AND
                     MServiceLimit.DialType = ServiceLimit.DialType AND
                     MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
                     MServiceLimit.EndTS <= Func.Common:mMakeTS() NO-LOCK NO-ERROR.
          IF NOT AVAIL MServiceLimit THEN DO:
             FOR EACH MsOwner NO-LOCK WHERE
                      MsOwner.MsSeq = MobSub.MsSeq USE-INDEX MsSeq:

                IF MsOwner.CLIType NE MobSub.CLIType THEN LEAVE.
                ldeActStamp = MsOwner.TSBegin.
             END. /* FOR EACH MsOwner NO-LOCK WHERE */
             Func.Common:mSplitTS(ldeActStamp, OUTPUT ldaActDate, OUTPUT liActTime).
          END.
          ELSE DO:
             ldeActStamp = MServiceLimit.EndTS.
             Func.Common:mSplitTS(ldeActStamp, OUTPUT ldaActDate, OUTPUT liActTime).
          END.
       END.
       ELSE DO:
          ldeActStamp = MServiceLimit.FromTS.
          Func.Common:mSplitTS(ldeActStamp, OUTPUT ldaActDate, OUTPUT liActTime).
          
          IF TODAY EQ ldaActDate THEN
            ldaRenewalDate = Func.Common:mLastDayOfMonth(TODAY) + 1.
          /* In case of renewal day and if renewal EDR is received,
             show next month renewal date */ 
          ELSE IF (DAY(TODAY) EQ DAY(ldaActDate) OR
                  (DAY(ldaActDate) > DAY(TODAY) AND
                   Func.Common:mLastDayOfMonth(TODAY) EQ TODAY)) THEN DO:
            IF CAN-FIND (FIRST PrepEDR NO-LOCK WHERE
                               PrepEDR.MsSeq = MobSub.Msseq AND
                               PrepEDR.DateST = TODAY AND
                               PrepEDR.SuccessCode EQ 1 AND
                               PrepEDR.CLIType EQ MobSub.CLIType AND
                               PrepEDR.ErrorCode EQ 0) THEN
            ldaRenewalDate = Func.Common:mLastDayOfMonth(TODAY) + 1.
            ELSE ldaRenewalDate = TODAY.
          END.
          ELSE IF DAY(TODAY) < DAY(ldaActDate) THEN ldaRenewalDate = TODAY.
          ELSE ldaRenewalDate = Func.Common:mLastDayOfMonth(TODAY) + 1.
             
          IF DAY(Func.Common:mLastDayOfMonth(ldaRenewalDate)) >= DAY(ldaActDate) THEN
             ldaRenewalDate = DATE(MONTH(ldaRenewalDate),
                                   DAY(ldaActDate),
                                   YEAR(ldaRenewalDate)).
          ELSE ldaRenewalDate = Func.Common:mLastDayOfMonth(ldaRenewalDate). 
          ldaActDate = ldaRenewalDate.
       END.

       IF ldaActDate <> ? THEN
          add_datetime(resp_struct,"tarj7_reset_date", ldaActDate).
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

FIND IMSI NO-LOCK WHERE IMSI.ICC = mobsub.ICC NO-ERROR.
IF AVAILABLE IMSI THEN DO:
    add_string(resp_struct, "pin1", IMSI.PIN1).
    add_string(resp_struct, "pin2", IMSI.PIN2).
    add_string(resp_struct, "puk1", IMSI.PUK1).
    add_string(resp_struct, "puk2", IMSI.PUK2).
END.

fGetLimit(MobSub.InvCust, MobSub.MsSeq, {&LIMIT_TYPE_BILLPERM}, 0, 0, TODAY).
add_int(resp_struct,"billing_permission",
   (IF AVAIL Limit THEN INT(Limit.LimitAmt) ELSE 0)).

add_string(resp_struct, "id_code", MobSub.IDCode).

/* Check if subscription has MNP OUT processes */
liMNPOutExists = 0.
MNPSUB_LOOP:
FOR EACH MNPSub NO-LOCK WHERE
         MNPSub.MsSeq = MobSub.MsSeq,
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

/* orders */
order_array = add_array(resp_struct,"orders").
liMNPStatus = ?.
FOR EACH Order NO-LOCK WHERE
         Order.MsSeq = MobSub.MsSeq AND
         Order.OrderType <= {&ORDER_TYPE_STC} BY Order.CrStamp:

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

   IF Order.StatusCode EQ "6" AND
      Order.OrderType < 2 THEN liMNPStatus = Order.MNPStatus.
END.

/* NEW or MNP number */
IF liMNPStatus NE ? THEN
   add_string(resp_struct, "number_type",
      STRING(liMNPStatus EQ 0, "new/mnp")).
ELSE  
   add_string(resp_struct, "number_type",
      STRING(((fISYoigoCLI(MobSub.CLI) AND llYoigoTenant) OR (fIsMasmovilCLI(MobSub.CLI) AND llMasmovilTenant)), "new/mnp")).

/* subscription terminals  */
term_array = add_array(resp_struct,"sub_terminals").
laptop_array = add_array(resp_struct,"sub_laptops").
FOR EACH SubsTerminal NO-LOCK WHERE 
         SubsTerminal.MsSeq = MobSub.MsSeq USE-INDEX MsSeq:

   CASE SubsTerminal.TerminalType:
   
      WHEN {&TERMINAL_TYPE_PHONE} THEN DO:
         term_struct = add_struct(term_array,"").
         add_string(term_struct,"imei", SubsTerminal.IMEI).
         add_string(term_struct,"billing_item_id", SubsTerminal.BillCode). 
         add_int(term_struct,"sub_terminal_id", SubsTerminal.TerminalId). 
         add_timestamp(term_struct,"delivered",SubsTerminal.PurchaseTS).
      END.
   
      WHEN {&TERMINAL_TYPE_LAPTOP} THEN DO:
         term_struct = add_struct(laptop_array,"").
         add_string(term_struct,"serial_number", SubsTerminal.IMEI).
         add_string(term_struct,"laptop", SubsTerminal.BillCode). 
         add_timestamp(term_struct,"ordered",SubsTerminal.PurchaseTS).
      END.
   END.
END.

/* active payterm periodical contract */
/* NOTE: it is supposed that there is only one active installment contract */

installment_array = add_array(resp_struct, "installments").

IF NOT MobSub.PayType THEN DO:


   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq = MobSub.MsSeq AND
            DCCLI.DCEvent BEGINS "PAYTERM" AND
            DCCLI.ValidTo >= TODAY BY DCCLI.ValidFrom:

      IF NOT fGetFixedFeeInfo(MobSub.MsSeq,
                              MobSub.Custnum,
                              DCCLI.DCEvent,
                              DCCLI.PerContractId,
                              ?,
                              OUTPUT ldePendingFee,
                              OUTPUT liTotalPeriods,
                              OUTPUT ldePeriodFee,
                              OUTPUT lderesidualFee,
                              OUTPUT lcFinancedInfo,
                              OUTPUT liOrderId) THEN NEXT.

      payterm_struct = add_struct(installment_array,"").
      add_double(payterm_struct,"pending_fee", ldePendingFee).
      add_int(payterm_struct,"per_contract_id", DCCLI.PerContractId).

      ldeFinalAmt = 0.
      FOR EACH SingleFee USE-INDEX Custnum WHERE
               SingleFee.Brand       = Syst.Var:gcBrand AND
               SingleFee.Custnum     = Mobsub.InvCust AND
               SingleFee.HostTable   = "Mobsub" AND
               SingleFee.KeyValue    = STRING(Mobsub.MsSeq) AND
               SingleFee.SourceTable = "DCCLI" AND
               SingleFee.SourceKey   = STRING(DCCLI.PerContractId) AND
               SingleFee.BillPeriod >= (YEAR(TODAY) * 100 + MONTH(TODAY)) AND
               SingleFee.CalcObj     = "RVTERM" NO-LOCK:
         ldeFinalAmt = ldeFinalAmt + SingleFee.Amt.
      END.

      add_double(payterm_struct,"final_fee", ldeFinalAmt).
   END.


   /* Count possible penalty fee for terminal contract termination */
   liCountMobile = 0.
   liCountFixed  = 0.

   CONTRACT_LOOP:
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq = MobSub.MsSeq AND
            DCCLI.ValidFrom <= TODAY AND
            DCCLI.ValidTo   >= TODAY AND
            DCCLI.CreateFees = TRUE,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeModel NE "" AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK BY DCCLI.ValidFrom DESC:
   
      IF DCCLI.DCEvent BEGINS "TERM" THEN DO:
         liCountMobile = liCountMobile + 1.
         IF liCountMobile > 1 THEN NEXT.

         add_datetime(resp_struct,"permanent_contract_valid_to",
            (IF DCCLI.Termdate NE ? THEN DCCLI.Termdate ELSE DCCLI.ValidTo)).

         add_datetime(resp_struct,"permanent_contract_valid_from",
            (IF DCCLI.RenewalDate NE ? THEN DCCLI.RenewalDate ELSE DCCLI.ValidFrom)).
      
         add_datetime(resp_struct,"permanent_contract_original_valid_to",
            (IF DCCLI.ValidToOrig NE ? THEN DCCLI.ValidToOrig ELSE DCCLI.ValidTo)).

         /* clitype at the moment of discount periodical contract creation */
         lcOrigCLIType = fGetCLITypeAtTermDiscount(BUFFER DCCLI). 
         IF lcOrigCLIType NE "" THEN 
            add_string(resp_struct,"original_subscription_type_id",lcOrigCLIType).
      END.
      ELSE IF DCCLI.DCEvent BEGINS "FTERM" AND 
         Mobsub.fixednumber NE ? THEN DO:
         liCountFixed = liCountFixed + 1.
         IF liCountFixed > 1 THEN NEXT.

         add_datetime(resp_struct,"fixed_permanent_contract_valid_to",
            (IF DCCLI.Termdate NE ? THEN DCCLI.Termdate ELSE DCCLI.ValidTo)).

         add_datetime(resp_struct,"fixed_permanent_contract_valid_from",
            (IF DCCLI.RenewalDate NE ? THEN DCCLI.RenewalDate ELSE DCCLI.ValidFrom)).
      
         add_datetime(resp_struct,"fixed_permanent_contract_original_valid_to",
            (IF DCCLI.ValidToOrig NE ? THEN DCCLI.ValidToOrig ELSE DCCLI.ValidTo)).

      END.

      IF liCountMobile GE 1 AND 
         liCountFixed  GE 1 THEN LEAVE CONTRACT_LOOP.
   END. /* CONTRACT_LOOP: */
END.

/* Check if subscription has any memos of specified type */
memo_struct = add_struct(resp_struct, "memo_counts").
add_int(memo_struct,"mobsub",  INT(fMemoCount("mobsub",MobSub.Msseq,True))).
add_int(memo_struct,"customer",INT(fMemoCount("customer",MobSub.Custnum,True))).
add_int(memo_struct,"invoice", INT(fMemoCount("invoice",MobSub.Custnum,True))).
add_int(memo_struct,"service", INT(fMemoCount("service",MobSub.Msseq,True))).


/* satisfaction value */
FIND FIRST PIndicator  WHERE
           PIndicator.Brand = Syst.Var:gcBrand AND
           PIndicator.HostTable = "MobSub" AND
           PIndicator.KeyValue = STRING(MobSub.MsSeq) AND
           PIndicator.IndicatorType = {&P_INDICATOR_TYPE_SATISFACTION_VALUE}  
           USE-INDEX HostTable NO-LOCK NO-ERROR. 
IF AVAIL PIndicator THEN 
    add_string(resp_struct,"satisfaction_value",PIndicator.IndicatorValue) .

IF fCLITypeIsExtraLine(MobSub.CLIType)
THEN DO:

    FIND FIRST lbMLMobSub NO-LOCK  WHERE 
               lbMLMobSub.Brand        = Syst.Var:gcBrand        AND
               lbMLMobSub.CustNum      = MobSub.CustNum          AND 
               lbMLMobSub.MsSeq        = MobSub.MultiSimId       AND 
              (lbMLMobSub.MsStatus     = {&MSSTATUS_ACTIVE}      OR
               lbMLMobSub.MsStatus     = {&MSSTATUS_BARRED}) NO-ERROR.
   
    IF AVAIL lbMLMobSub 
    THEN ASSIGN
         liMultiSimTypeValue = {&MULTISIMTYPE_PRIMARY} 
         lcMultiSimCLI       = lbMLMobSub.FixedNumber + " / " + lbMLMobSub.CLI.
         
         
    add_int(resp_struct,"multisim_type", liMultiSimTypeValue) .
    add_string(resp_struct,"multisim_msisdn", lcMultiSimCLI).     
     
END.
ELSE IF fCLITypeIsMainLine(MobSub.CLIType)
THEN DO:
     
    FOR EACH lbELMobSub NO-LOCK WHERE
             lbELMobSub.Brand        = Syst.Var:gcBrand        AND
             lbELMobSub.CustNum      = MobSub.CustNum          AND
             lbELMobSub.MultiSimId   = MobSub.MsSeq            AND 
            (lbELMobSub.MsStatus     = {&MSSTATUS_ACTIVE}  OR
             lbELMobSub.MsStatus     = {&MSSTATUS_BARRED})     AND
             lbELMobSub.MultiSimType = {&MULTISIMTYPE_EXTRALINE} :
             
        ASSIGN  
            liMultiSimTypeValue =   {&MULTISIMTYPE_EXTRALINE}
            lcMultiSimCLI       =   lcMultiSimCLI + ";" + lbELMobSub.CLI .  
           
    END.
    
    ASSIGN lcMultiSimCLI = TRIM(lcMultiSimCLI,";") .   
    
    add_int(resp_struct,"multisim_type", liMultiSimTypeValue) .
    add_string(resp_struct,"multisim_msisdn", lcMultiSimCLI).        
        
END.
ELSE IF (MobSub.MultiSIMType > 0 AND
         MobSub.MultiSIMID   > 0 AND
         Mobsub.MultiSimType NE {&MULTISIMTYPE_EXTRALINE})  
 THEN DO:
    
    liMultiSIMType = (IF MobSub.MultiSIMType EQ {&MULTISIMTYPE_PRIMARY} THEN {&MULTISIMTYPE_SECONDARY} ELSE {&MULTISIMTYPE_PRIMARY} ).
    
    FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSimID WHERE
               lbMobSub.Brand        = Syst.Var:gcBrand  AND
               lbMobSub.MultiSimID   = MobSub.MultiSimID AND
               lbMobSub.MultiSimType = liMultiSIMType    AND
               lbMobSub.Custnum      = MobSub.Custnum    NO-ERROR.     
               
    IF AVAIL lbMobSub  
    THEN ASSIGN 
         liMultiSimTypeValue = MobSub.MultiSimType
         lcMultiSimCLI       = lbMobSub.CLI.
         
    add_int(resp_struct,"multisim_type", liMultiSimTypeValue) .
    add_string(resp_struct,"multisim_msisdn", lcMultiSimCLI).  
    
    IF AVAIL lbMobSub THEN 
    DO:
        /* Return warning flag for secondary line */
        IF MobSub.MultiSIMType = {&MULTISIMTYPE_PRIMARY} AND
            NOT CAN-FIND (FIRST MsRequest WHERE
            MsRequest.MsSeq   = lbMobSub.Msseq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus),
            {&REQ_INACTIVE_STATUSES}) = 0) AND
            NOT CAN-FIND (FIRST MsRequest WHERE
            MsRequest.MsSeq   = lbMobSub.Msseq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
            LOOKUP(STRING(MsRequest.ReqStatus),
            {&REQ_INACTIVE_STATUSES}) = 0) AND
            NOT Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT lbMobSub.CLI) THEN
            add_boolean(resp_struct,"multisim_warning_for_secondary",TRUE).

        /* Return warning flag for secondary line if primary line outporting */
        ELSE IF MobSub.MultiSIMType = {&MULTISIMTYPE_SECONDARY} AND
                NOT CAN-FIND (FIRST MsRequest WHERE
                MsRequest.MsSeq   = MobSub.Msseq AND
                MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                LOOKUP(STRING(MsRequest.ReqStatus),
                {&REQ_INACTIVE_STATUSES}) = 0) AND
                NOT CAN-FIND (FIRST MsRequest WHERE
                MsRequest.MsSeq   = MobSub.Msseq AND
                MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                LOOKUP(STRING(MsRequest.ReqStatus),
                {&REQ_INACTIVE_STATUSES}) = 0) AND
                NOT Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT MobSub.CLI) AND
                Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT lbMobSub.CLI) THEN
                add_boolean(resp_struct,"multisim_warning_for_secondary",TRUE).   
         
    END.            
  
END.

FINALLY:
   END.
