/**
 * create Discount for a specific mobsub
 * 
 *
 * @input msseq;int;mandatory;id of subscription
 *        username;string;mandatory; who create the Discount
          discount;structure;mandatory; discount details          
 * @discount id;string;mandatory; Discount Plan Rule ID
             disc_value;decimal;mandatory; Amount of discount
             valid_from;date;mandatory; Discount start date
             valid_periods;int;mandatory; Discount duration
             discount_monthly_limit;double;mandatory;Discount amount monthly limit
             permanency;string;optional;Periodical contract
  * @output ret;boolean; Return value
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Mc/dpmember.i}
{Syst/tmsconst.i}
{Func/fcounter.i}
{Func/fixedlinefunc.i}
{Func/fmakemsreq.i}

/* Input parameters */
DEFINE VARIABLE piMsSeq          AS INTEGER   NO-UNDO. 
DEFINE VARIABLE pcUserName       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStruct         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStructType     AS CHARACTER NO-UNDO.

/* Output parameters */
/* Local variables */
DEFINE VARIABLE lcDPRuleID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeAmount        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldeMaxAmount     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldaValidFrom     AS DATE      NO-UNDO.
DEFINE VARIABLE ldaValidTo       AS DATE      NO-UNDO.
DEFINE VARIABLE liValidPeriods   AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeMonthlyLimit  AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE ldeMonthAmt      AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE ldeMonthFrom     AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE ldeMonthTo       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcPerContract    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcError          AS CHARACTER NO-UNDO.
DEFINE VARIABLE liResult         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcResult         AS CHARACTER NO-UNDO.

/* ALFMO-14 for web memo creation */
DEFINE VARIABLE lcMainLine    AS CHARACTER NO-UNDO.

DEFINE VARIABLE liDiscCreDelay  AS INTEGER NO-UNDO.
DEFINE VARIABLE ldActStamp      AS DECIMAL NO-UNDO.
DEFINE VARIABLE lcList          AS CHAR NO-UNDO. /* list From Cparam */
DEFINE VARIABLE i               AS INT NO-UNDO.

DEFINE BUFFER bDiscountPlan FOR DiscountPlan.
DEFINE BUFFER bMXItem       FOR MXItem.

lcStructType = validate_request(param_toplevel_id, "int,string,struct,[string]").
IF lcStructType EQ ? THEN RETURN.

/* ALFMO-14 Procedure returns the convergent main line MSISDN 
   For web memo creation */
PROCEDURE pConvMainLine :
   DEFINE INPUT  PARAMETER icCustIDType  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCustID      AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCliType     AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER ocMainLineCli AS CHAR NO-UNDO.   

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.
   DEFINE BUFFER bClitype  FOR Clitype.

   for-blk:
   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Var:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Var:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.PayType = FALSE                   AND
            (bMobSub.MsStatus = {&MSSTATUS_ACTIVE}     OR
             bMobSub.MsStatus = {&MSSTATUS_BARRED}),
       FIRST bCliType WHERE bCliType.Brand = Syst.Var:gcBrand AND bCliType.CliType = bMobSub.CliType NO-LOCK:
      
      IF bCliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN 
          NEXT.

      IF fIsConvergentAddLineOK(bMobSub.CLIType,icCliType) THEN 
      DO:
         ASSIGN ocMainLineCli = bMobSub.cli.
         LEAVE for-blk.
      END.
   END.   

END PROCEDURE.

/* ALFMO-14 Procedure returns the Mobile only main line MSISDN 
   For web memo creation */
PROCEDURE pMobOnlyMainLine :
   DEFINE INPUT  PARAMETER icCustIDType  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCustID      AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCliType     AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER ocMainLineCli AS CHAR NO-UNDO.   

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Var:gcBrand AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(icCliType, {&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.
  
   for-blk:
   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Var:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Var:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.MsSeq  <> MobSub.MsSeq            AND
             bMobSub.PayType = FALSE:

       IF (icCliType = ENTRY(3,{&ADDLINE_CLITYPES} ) OR 
           icCliType = ENTRY(4,{&ADDLINE_CLITYPES} )) AND 
          CAN-FIND(FIRST DPMember WHERE
                         DPMember.DPId = DiscountPlan.DPId AND
                         DPMember.HostTable = "MobSub" AND
                         DPMember.KeyValue  = STRING(bMobSub.MsSeq) AND
                         DPMember.ValidTo   >= TODAY) THEN NEXT.

       IF fIsMobileOnlyAddLineOK(bMobSub.CLIType,icCliType) THEN
       DO:
          ASSIGN ocMainLineCli = bMobSub.cli.
          LEAVE for-blk.
       END.          
   END.

END PROCEDURE.

piMsSeq = get_int(param_toplevel_id, "0").
pcUserName = "VISTA_" + get_string(param_toplevel_id, "1").
pcStruct = get_struct(param_toplevel_id,"2").

lcStruct = validate_struct(pcStruct, "id!,disc_value!,valid_from!," +
                                     "valid_periods!,discount_monthly_limit!," +
                                     "permanency").


lcDPRuleID     = get_string(pcStruct, "id").
ldeAmount      = get_double(pcStruct, "disc_value").
ldaValidFrom   = get_date(pcStruct, "valid_from").
liValidPeriods = get_int(pcStruct, "valid_periods").
ldeMonthlyLimit = get_double(pcStruct, "discount_monthly_limit").

ASSIGN lcPerContract = get_string(pcStruct, "permanency") WHEN
         LOOKUP ("permanency", lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcDPRuleID = ENTRY(1,lcDPRuleID,"|").

/* Validations for amount and valid peridos: */
IF ldeAmount <= 0 THEN RETURN appl_err("Invalid Discount amount").

IF liValidPeriods <= 0 THEN RETURN appl_err("ValidPeriod must be one month at minimum").

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").
Syst.Var:katun = pcUserName.

{newton/src/findtenant.i NO OrderCanal MobSub MsSeq piMsSeq}

FIND Customer OF Mobsub NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN
   RETURN appl_err("Customer not available").

/* ALFMO-14 Additional line mobile only */
IF lcDPRuleID = "additional_line_discount" THEN
DO:
   IF LOOKUP(MobSub.CliType,{&ADDLINE_CLITYPES}) = 0 THEN
      RETURN appl_err("Discount Plan not allowed").

   IF fCheckExistingConvergent(Customer.CustIDType,
                               Customer.OrgID,
                               MobSub.CliType) THEN
   DO:
      RUN pConvMainLine(Customer.CustIDType,
                        Customer.OrgID,
                        MobSub.CliType, 
                        OUTPUT lcMainLine).

      lcDPRuleID = ENTRY(LOOKUP(MobSub.CliType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS}).
   END.
   ELSE IF fCheckExistingMobileOnly(Customer.CustIDType,
                                    Customer.OrgID,
                                    MobSub.CliType) THEN
   DO:
      RUN pMobOnlyMainLine(Customer.CustIDType,
                           Customer.OrgID,
                           MobSub.CliType, 
                           OUTPUT lcMainLine).
      
      lcDPRuleID = ENTRY(LOOKUP(MobSub.CliType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_HM}).
   END.
   IF lcMainLine = "" THEN
      RETURN appl_err("Discount Plan not allowed").
END.

FIND FIRST DiscountPlan WHERE
           DiscountPlan.Brand = Syst.Var:gcBrand AND
           DiscountPlan.DPRuleID = lcDPRuleID NO-LOCK NO-ERROR.
IF NOT AVAILABLE DiscountPlan THEN
   RETURN appl_err("Unknown Discount Plan").

IF liValidPeriods = 999 THEN
   ldaValidTo = 12/31/2049.
ELSE
   ldaValidTo = fCalcDPMemberValidTo(ldaValidFrom, liValidPeriods).

/* YCO-468. Periodical contract for Permanency. Validation. */
IF lcPerContract <> "" THEN DO:
   /* Does Periodical contract exist? */
   FIND FIRST DayCampaign NO-LOCK WHERE 
              DayCampaign.Brand   EQ Syst.Var:gcBrand AND 
              DayCampaign.DCEvent EQ lcPerContract
              USE-INDEX DCEvent NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN 
      RETURN appl_err("Unknown Periodical Contract " + lcPerContract). 
     
   /* YTS-13260 - Is Periodical Contract allowed for this subscription type? */    
   IF NOT CAN-FIND(FIRST MXItem WHERE 
                         MXItem.MXValue EQ MobSub.CliType  AND  
                         MXItem.MXName  EQ "SubsTypeTo"    AND 
                         CAN-FIND(FIRST bMXItem WHERE 
                                        bMXItem.MxSeq   EQ MXItem.MXSeq  AND 
                                        bMXItem.MxName  EQ "PerContract" AND 
                                        bMXItem.MxValue EQ lcPerContract)) THEN                                                                             
      RETURN appl_err("Periodical Contract Not Allowed: " + lcPerContract). 
END.

/* ALFMO-14 Additional Line with mobile only ALFMO-5 */
IF LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS_HM}) > 0 THEN 
DO:      

   IF fCheckOngoingConvergentOrder(Customer.CustIDType,
                                   Customer.OrgID,
                                   MobSub.CliType) THEN      
      RETURN appl_err("Discount Plan not allowed").      
END.

/* ALFMO-14 Additional line mobile only */
IF LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS_HM}) > 0 OR
   LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS}) > 0 THEN
DO:
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq = MobSub.MsSeq AND
            DCCLI.DCEvent BEGINS "TERM" AND
            DCCLI.ValidTo >= TODAY AND
            DCCLI.ValidFrom <= TODAY AND
            DCCLI.CreateFees = TRUE,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeModel NE "" AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK BY DCCLI.ValidFrom DESC:
      RETURN appl_err("Customer has active permanency contract").
   END.
END.

/* YCO-458. Validation about existing additional lines */
/* discounts removed, as far as now we are checking    */
/* "incompatible discount matrix" (TMSRelation table)  */
/* in fAddDiscountPlanMember (dpmember.i)              */ 

IF DiscountPlan.DPUnit = "Percentage" THEN
   ldeMaxAmount = DiscountPlan.MaxAmount.
ELSE ldeMaxAmount = ldeAmount.
 
/* check monthly limits */
Func.Common:mMonthlyStamps(TODAY,
               OUTPUT ldeMonthFrom,
               OUTPUT ldeMonthTo). 

FOR EACH Counter NO-LOCK WHERE 
         Counter.Brand = Syst.Var:gcBrand AND
         Counter.HostTable = "MobSub" AND
         Counter.KeyValue = STRING(MobSub.MsSeq) AND
         Counter.CounterType = {&COUNTERTYPE_DISCOUNT_AMOUNT} AND
         Counter.EndStamp <= ldeMonthTo AND
         Counter.BeginStamp >= ldeMonthFrom : 
       ldeMonthAmt = ldeMonthAmt + Counter.CounterAmt.
END.
IF ( ldeMonthAmt + ldeMaxAmount) > ldeMonthlyLimit THEN
       RETURN appl_err("Change exceeds the monthly limit ").

lcError = fAddDiscountPlanMember(MobSub.MsSeq,
                                 DiscountPlan.DPRuleID,
                                 ldeAmount,
                                 ldaValidFrom,
                                 ldaValidTo,
                                 ?,
                                 0). /* OrderId */

IF lcError BEGINS "ERROR"
THEN RETURN appl_err(lcError).

/* YCO-468. Assign permanency when lcPerContract <> "" */
IF lcPerContract <> "" THEN DO:

   /* YCO-757. Delay for permanency */
   lcList = Syst.Parameters:getc("DelayedPermanencies", "Discount").
   IF LOOKUP(lcPerContract, lcList) > 0 THEN DO:
      liDiscCreDelay = Syst.Parameters:geti("DelayPermanencyValue", "Discount").
      /* def = 0 current functionality without delay. For YCO-757 def value is 432000 */
      ldActStamp = Func.Common:mSecOffSet(Func.Common:mMakeTS(),liDiscCreDelay).
   END. 
      
   liResult = fPCActionRequest(
                        Mobsub.MsSeq,             /* subscription */
                        lcPerContract,            /* DayCampaign.DCEvent */
                        "act",                    /* act,term,canc,iterm,cont */
                        /* 0, */ ldActStamp,      /* when request should be handled, 0 --> Now */
                        TRUE,                     /* fees */
                        {&REQUEST_SOURCE_NEWTON}, /* where created */
                        pcUserName,               /* creator  */
                        0,                        /* main request  */
                        FALSE,                    /* main request waits for this */
                        "",                       /* sms                  */
                        0,                        /* payterm residual fee */
                        0,                        /* Periodical Contract-ID  */ 
                        "",                       /* Parameters to be stored for SVA       */
                        OUTPUT lcResult).
   IF liResult EQ 0 THEN 
      RETURN appl_err(lcResult).   
END.    

/* ALFMO-14 For creating web memo */
IF LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS_HM}) > 0 OR
   LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS}) > 0 THEN
DO:
   Func.Common:mWriteMemo("Invoice",
              STRING(MobSub.MsSeq),
              (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0),
              "Descuento 50% línea adicional",
              "Línea principal " + lcMainLine).
END.
 
/* YTS-10992 - Adding logging for dpmember creation 
   (Using already available Memo creation function instead of 
    including event creation logic) */
Func.Common:mWriteMemo("MobSub",
           STRING(MobSub.MsSeq),
           (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0),
           "DiscountCreation",
           "Added from Vista").

/* update/create the counter */
fUpdateCounter("MobSub",
               STRING(MobSub.MsSeq),
               {&COUNTERTYPE_DISCOUNT_AMOUNT},
               ldeMonthTo,
               ldeMonthFrom,
               ldeMaxAmount).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   END.
