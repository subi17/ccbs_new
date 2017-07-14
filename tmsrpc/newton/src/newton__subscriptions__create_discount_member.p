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
  * @output ret;boolean; Return value
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcbrand = "1".
{Mc/dpmember.i}
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/fcounter.i}
{Func/fixedlinefunc.i}

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
/* ALFMO-14 for web memo creation */
DEFINE VARIABLE lcMainLine    AS CHARACTER NO-UNDO.

DEFINE BUFFER bDiscountPlan FOR DiscountPlan.

lcStructType = validate_request(param_toplevel_id, "int,string,struct,[boolean]").
IF lcStructType EQ ? THEN RETURN.

FUNCTION fLocalMemo RETURNS LOGIC
   (icHostTable AS CHAR,
    icKey       AS CHAR,
    icTitle     AS CHAR,
    icText      AS CHAR):

   CREATE Memo.
   ASSIGN
      Memo.Brand     = gcBrand
      Memo.CreStamp  = fMakeTS()
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.Custnum   = (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0)
      Memo.HostTable = icHostTable
      Memo.KeyValue  = icKey
      Memo.CreUser   = katun
      Memo.MemoTitle = icTitle
      Memo.Memotext  = icText.
      
END FUNCTION.

/* ALFMO-14 Procedure returns the convergent main line MSISDN 
   For web memo creation */
PROCEDURE fConvMainLine :
   DEFINE INPUT  PARAMETER icCustIDType  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCustID      AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCliType     AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER ocMainLineCli AS CHAR NO-UNDO.   

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.
   DEFINE BUFFER bClitype  FOR Clitype.

   for-blk:
   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Parameters:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Parameters:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.PayType = FALSE                   AND
            (bMobSub.MsStatus = {&MSSTATUS_ACTIVE}     OR
             bMobSub.MsStatus = {&MSSTATUS_BARRED}),
       FIRST bCliType WHERE bCliType.Brand = Syst.Parameters:gcBrand AND bCliType.CliType = bMobSub.CliType NO-LOCK:
      
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
PROCEDURE fMobOnlyMainLine :
   DEFINE INPUT  PARAMETER icCustIDType  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCustID      AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCliType     AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER ocMainLineCli AS CHAR NO-UNDO.   

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Parameters:gcBrand AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(icCliType, {&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.
  
   for-blk:
   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Parameters:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Parameters:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.PayType = FALSE:

       /* This is to handle where the additional line
          is CONT25 or CONT26 because it can treat itself
          as main line */  
       IF (bMobSub.CLIType = ENTRY(3,{&ADDLINE_CLITYPES} ) OR 
           bMobSub.CLIType = ENTRY(4,{&ADDLINE_CLITYPES} )) AND 
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
                                     "valid_periods!,discount_monthly_limit!").

lcDPRuleID     = get_string(pcStruct, "id").
ldeAmount      = get_double(pcStruct, "disc_value").
ldaValidFrom   = get_date(pcStruct, "valid_from").
liValidPeriods = get_int(pcStruct, "valid_periods").
ldeMonthlyLimit = get_double(pcStruct, "discount_monthly_limit").

IF gi_xmlrpc_error NE 0 THEN RETURN.

/* Validations for amount and valid peridos: */
IF ldeAmount <= 0 THEN RETURN appl_err("Invalid Discount amount").

IF liValidPeriods <= 0 THEN RETURN appl_err("ValidPeriod must be one month at minimum").

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").
katun = pcUserName.

/* Check that mobsub is available */
FIND MobSub WHERE
     MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN appl_err("Mobile Subscription not available").

FIND Customer OF Mobsub NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN
   RETURN appl_err("Customer not available").

/* ALFMO-14 Additional line mobile only */
IF lcDPRuleID = "additional_line_discount" THEN
DO:
   IF LOOKUP(MobSub.CliType,{&ADDLINE_CLITYPES}) = 0 THEN
      RETURN appl_err("Discount Plan not allowed").

   IF fCheckExistingConvergent(Customer.CustIDType,Customer.OrgID,MobSub.CliType) THEN
      lcDPRuleID = ENTRY(LOOKUP(MobSub.CliType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS}).
   ELSE IF fCheckExistingMobileOnly(Customer.CustIDType,Customer.OrgID,MobSub.CliType) THEN
      lcDPRuleID = ENTRY(LOOKUP(MobSub.CliType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_HM}).

END.

FIND FIRST DiscountPlan WHERE
           DiscountPlan.Brand = gcBrand AND
           DiscountPlan.DPRuleID = lcDPRuleID NO-LOCK NO-ERROR.
IF NOT AVAILABLE DiscountPlan THEN
   RETURN appl_err("Unknown Discount Plan").

IF liValidPeriods = 999 THEN
   ldaValidTo = 12/31/2049.
ELSE
   ldaValidTo = fCalcDPMemberValidTo(ldaValidFrom, liValidPeriods).

/* ALFMO-14 Additional Line with mobile only ALFMO-5 */
IF LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS_HM}) > 0 THEN DO:      

   IF fCheckOngoingConvergentOrder(Customer.CustIDType,Customer.OrgID,MobSub.CliType) OR 
      CAN-FIND(FIRST SubsTerminal WHERE SubsTerminal.Brand = gcBrand AND
                                        SubsTerminal.MsSeq = MobSub.MsSeq ) THEN      
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
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeModel NE "" AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK BY DCCLI.ValidFrom DESC:
      RETURN appl_err("Customer has active permanency contract").
   END.
END.

FOR EACH bDiscountPlan NO-LOCK WHERE
         bDiscountPlan.Brand = gcBrand AND
  LOOKUP(bDiscountPlan.DPRuleID, {&ADDLINE_DISCOUNTS} + "," + {&ADDLINE_DISCOUNTS_20} + "," + {&ADDLINE_DISCOUNTS_HM}) > 0,
  FIRST DPMember NO-LOCK WHERE
        DPMember.DPId       = bDiscountPlan.DPId   AND
        DPMember.HostTable  = "MobSub"             AND
        DPMember.KeyValue   = STRING(MobSub.MsSeq) AND
        DPMember.ValidTo   >= ldaValidFrom         AND
        DPMember.ValidFrom <= ldaValidTo:
   RETURN appl_err("Discount Plan already exists").
END.

FIND FIRST DPMember WHERE
           DPMember.DPId = DiscountPlan.DPId AND
           DPMember.HostTable = "MobSub" AND
           DPMember.KeyValue  = STRING(MobSub.MsSeq) AND
           DPMember.ValidTo >= ldaValidFrom AND
           DPMember.ValidFrom <= ldaValidTo NO-LOCK NO-ERROR.

IF AVAILABLE DPMember THEN
   RETURN appl_err("Discount Plan already exists").

IF DiscountPlan.DPUnit = "Percentage" THEN
   ldeMaxAmount = DiscountPlan.MaxAmount.
ELSE ldeMaxAmount = ldeAmount.
 
/* check monthly limits */
fMonthlyStamps(TODAY,
               OUTPUT ldeMonthFrom,
               OUTPUT ldeMonthTo). 

FOR EACH Counter NO-LOCK WHERE 
         Counter.Brand = gcBrand AND
         Counter.HostTable = "MobSub" AND
         Counter.KeyValue = STRING(MobSub.MsSeq) AND
         Counter.CounterType = {&COUNTERTYPE_DISCOUNT_AMOUNT} AND
         Counter.EndStamp <= ldeMonthTo AND
         Counter.BeginStamp >= ldeMonthFrom : 
       ldeMonthAmt = ldeMonthAmt + Counter.CounterAmt.
END.
IF ( ldeMonthAmt + ldeMaxAmount) > ldeMonthlyLimit THEN
       RETURN appl_err("Change exceeds the monthly limit ").

/* ALFMO-14 For creating web memo */
IF LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS}) > 0 THEN
DO:
   RUN fConvMainLine(Customer.CustIDType,Customer.OrgID,MobSub.CliType, OUTPUT lcMainLine).
END.
ELSE IF LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS_HM}) > 0 THEN
DO:
   RUN fMobOnlyMainLine(Customer.CustIDType,Customer.OrgID,MobSub.CliType, OUTPUT lcMainLine).
END.

CREATE DPMember.
ASSIGN 
   DPMember.DPMemberID = NEXT-VALUE(DPMemberID)
   DPMember.DPId      = DiscountPlan.DPId
   DPMember.HostTable = "MobSub" 
   DPMember.KeyValue  = STRING(MobSub.MsSeq) 
   DPMember.ValidFrom = ldaValidFrom
   DPMember.ValidTo   = ldaValidTo
   DPMember.DiscValue = ldeAmount.

/* ALFMO-14 For creating web memo */
IF LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS_HM}) > 0 OR
   LOOKUP(lcDPRuleID, {&ADDLINE_DISCOUNTS}) > 0 THEN
DO:
   fLocalMemo("MobSub",
              STRING(MobSub.MsSeq),
              "Descuento 50% línea adicional",
              "Línea principal " + lcMainLine).
END.
 
/* update/create the counter */
fUpdateCounter("MobSub",
               STRING(MobSub.MsSeq),
               {&COUNTERTYPE_DISCOUNT_AMOUNT},
               ldeMonthTo,
               ldeMonthFrom,
               ldeMaxAmount).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
