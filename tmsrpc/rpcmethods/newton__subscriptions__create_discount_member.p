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

FIND FIRST DiscountPlan WHERE
           DiscountPlan.Brand = gcBrand AND
           DiscountPlan.DPRuleID = lcDPRuleID NO-LOCK NO-ERROR.
IF NOT AVAILABLE DiscountPlan THEN
   RETURN appl_err("Unknown Discount Plan").

ldaValidTo = fCalcDPMemberValidTo(ldaValidFrom, liValidPeriods).

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

CREATE DPMember.
ASSIGN 
   DPMember.DPId      = DiscountPlan.DPId
   DPMember.HostTable = "MobSub" 
   DPMember.KeyValue  = STRING(MobSub.MsSeq) 
   DPMember.ValidFrom = ldaValidFrom
   DPMember.ValidTo   = ldaValidTo
   DPMember.DiscValue = ldeAmount.
 
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
