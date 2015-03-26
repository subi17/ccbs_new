/* ----------------------------------------------------------------------
  MODULE .......: Charge tool dialog 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 20.04.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{tmsconst.i}
{fcustpl.i}
{fuserright.i}
{lib/tokenlib.i}
{fcharge_comp_loaded.i}

DEFINE INPUT PARAMETER iiMsSeq AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER icOperation AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER odeCharge AS DECIMAL NO-UNDO.

DEFINE VAR ok AS LOGICAL NO-UNDO.
DEFINE VAR ldeCharge AS DECIMAL NO-UNDO.
DEFINE VAR lcPriceList AS CHARACTER NO-UNDO. 
DEFINE VAR llAdmin AS LOGICAL NO-UNDO.
DEFINE VAR ldChargeLimit AS DECIMAL NO-UNDO. 
DEFINE VAR ldChargeMonthLimit AS DECIMAL NO-UNDO.
DEFINE VARIABLE ldCurrBal AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE ldMonthLoaded AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE llNegative AS LOGICAL NO-UNDO.

IF getTMSRight("CCSUPER,SYST") EQ "RW" THEN llAdmin = TRUE.

FORM
    "Charge:" ldeCharge 
       HELP "Enter charge"
       FORMAT ">>9.99" SKIP
    WITH CENTERED ROW 8 TITLE COLOR VALUE(ctc) " DEFINE CHARGE "
    NO-LABELS OVERLAY FRAME f1.

ok = FALSE.
MESSAGE "Do you want to create charge (Y/N) ?" VIEW-AS ALERT-BOX QUESTION 
   BUTTONS YES-NO UPDATE ok.
IF NOT ok THEN RETURN.

/* check Mobsub, Customer,FeeModel */
FIND Mobsub WHERE Mobsub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL Mobsub THEN DO:
   MESSAGE "Subscription not found" VIEW-AS ALERT-BOX TITLE "SYSTEM ERROR".
   RETURN.
END.

FIND FeeModel WHERE 
     FeeModel.Brand = gcBrand AND
     FeeModel.FeeModel = icOperation NO-LOCK NO-ERROR.
IF NOT AVAIL FeeModel THEN DO:
   MESSAGE "Charge/Compensation Billing Event" icOperation "not found"
   VIEW-AS ALERT-BOX TITLE "SYSTEM ERROR".
   RETURN. 
END.

/* Fetch default charge */
lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                 MobSub.BillTarget,
                                 FeeModel.FeeModel,
                                 TODAY).

FIND FIRST FMItem NO-LOCK  WHERE
   FMItem.Brand     = gcBrand       AND
   FMItem.FeeModel  = FeeModel.FeeModel AND
   FMItem.PriceList = lcPriceList AND
   FMItem.FromDate <= TODAY     AND
   FMItem.ToDate   >= TODAY NO-ERROR.
   
IF AVAIL FMItem THEN ldeCharge = FMItem.Amount.
ELSE DO:
   MESSAGE "Charge/Compensation Billing Event" icOperation 
           "doesn't contain active item" VIEW-AS ALERT-BOX ERROR.
   RETURN. 
END.

IF FMItem.Amount >= 0 THEN llNegative = FALSE.  ELSE llNegative = TRUE.

 /* fetch user limits for charge and compensation */
ldChargeLimit = fUserLimitAmt(katun, (IF Mobsub.PayType = TRUE
                      THEN {&PREP_CHARGE_LIMIT_TYPE}
                      ELSE {&POST_CHARGE_LIMIT_TYPE})).
ldChargeMonthLimit = fUserLimitAmt(katun, (IF Mobsub.PayType = TRUE
                        THEN {&PREP_CHARGE_MONTHLY_LIMIT_TYPE}
                        ELSE {&POST_CHARGE_MONTHLY_LIMIT_TYPE})).

IF ldChargeLimit < 0 OR ldChargeMonthLimit < 0  THEN DO:
   MESSAGE "One time or monthly limit is not defined for your account and group"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

/* monthly loaded */
ldMonthLoaded = fMonthLoaded ( (IF ldeCharge > 0 THEN "CHARGE" ELSE "COMP"),
                               MobSub.CLI,
                               MobSub.PayType).

IF MobSub.PayType = TRUE AND ldeCharge > 0 THEN DO: /* charges for prepaid */

 /* get current balance */
   ldCurrBal = 0.
   RUN balancequery.p(MobSub.CLI).
   ldCurrBal = INT(RETURN-VALUE) / 100 NO-ERROR.
END.

IF NOT llAdmin THEN DO:
   
   /* charge/comp different of zero */
   IF ldeCharge = 0 THEN DO:
       MESSAGE "It is not possible to create charge/compensation" 
               "with zero value !"
       VIEW-AS ALERT-BOX ERROR.
       RETURN.
   END.

   /* monthly limit check */
   IF ABSOLUTE (ldeCharge + ldMonthLoaded) > ldChargeMonthLimit THEN DO:
      MESSAGE " Charge/compensation amount is over monthly limit"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   /* prepaid balance check */
   IF MobSub.PayType = TRUE AND ldeCharge > 0 AND ldeCharge > ldCurrBal THEN DO:
      MESSAGE "Charge should be less than current balance:" ldCurrBal "euros" 
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   /* otherwise everything is Ok */
   MESSAGE "Charge will be" ldeCharge  "euros" VIEW-AS ALERT-BOX TITLE "INFO".
   odeCharge = ldeCharge.
   RETURN.

END.

/* Change charge only for Admin users*/
ok = false.
ehto = 9. RUN ufkey.

LOOP:
REPEAT:
   DO WITH FRAME f1 ON ENDKEY UNDO, LEAVE LOOP:
      PAUSE 0.
      UPDATE ldeCharge WHEN llAdmin.
      if lookup(keylabel(lastkey),"f1,return") > 0 THEN DO:
         /* not zero charge/comp */
         IF ldeCharge = 0 THEN DO:
            MESSAGE "Value cannot be zero !"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         /* check that you keep the same sign */
         IF ldeCharge > 0 AND llNegative THEN DO:
            MESSAGE "Value should be negative"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         IF ldeCharge < 0 AND NOT llNegative THEN DO:
            MESSAGE "Value should be positive"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         /* one time limit check */
         IF ABSOLUTE (ldeCharge) > ldChargeLimit THEN DO:
            MESSAGE "Maximum"  ldChargeLimit  "charge in euros is allowed"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         /* monthly limit check */
         IF ABSOLUTE (ldeCharge + ldMonthLoaded) > ldChargeMonthLimit THEN DO:
            MESSAGE "Request is over monthly limit"  
                    "maximum" ldChargeMonthLimit - ABSOLUTE(ldMonthLoaded)
                    "is allowed"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         /* limit in current balance for prepaid */
         IF MobSub.PayType = TRUE AND 
            ldeCharge > 0 AND ldeCharge > ldCurrBal THEN DO:
            MESSAGE "Value should be lower than current balance:" 
               ldCurrBal  "euros "
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         ok = true.
         LEAVE LOOP.
      END.
   END.
END.

HIDE FRAME f1 NO-PAUSE.

IF ok THEN odeCharge = ldeCharge.
