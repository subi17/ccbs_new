/* ----------------------------------------------------------------------
  MODULE .......: fusion_stc_fallback.p
  TASK .........: Creates Fusion fallback STC request
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 27.09.13
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Mm/fbundle.i}

DEF INPUT PARAM piOrderID AS INT NO-UNDO. 
DEF OUTPUT PARAM oiRequest AS INT NO-UNDO. 

DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR ldeSTCTS AS DEC NO-UNDO. 
DEF VAR lcCLIType AS CHAR NO-UNDO. 
DEF VAR lcBundleID AS CHAR NO-UNDO. 
DEF VAR lcOrderBundleID AS CHAR NO-UNDO. 

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = Syst.Var:gcBrand AND
           Order.OrderID = piOrderID NO-ERROR.
IF NOT AVAIL Order THEN RETURN "Order not found".

IF NOT Order.OrderChannel BEGINS "fusion" THEN 
   RETURN "Order is not Fusion".

FIND FIRST Mobsub NO-LOCK WHERE
           Mobsub.Brand = Syst.Var:gcBrand AND
           Mobsub.Msseq = Order.MsSeq NO-ERROR.
IF NOT AVAIL Mobsub THEN RETURN "Subscription not found".

IF Order.CLIType NE Mobsub.CLIType THEN 
   RETURN "Order subscription type does not match with subscription".

FIND FIRST CLIType WHERE
           CLIType.Brand = Syst.Var:gcBrand AND
           CLIType.CLIType = Order.CLIType NO-LOCK NO-ERROR.
IF NOT AVAIL CLIType THEN RETURN "CLIType not found".
   
IF CLIType.BundleType = TRUE THEN DO:
   lcOrderBundleID = fGetDataBundleInOrderAction(Order.OrderID,Order.CLIType).
   IF lcOrderBundleID NE Mobsub.TariffBundle 
      THEN RETURN "Order tariff bundle does not match with subscription".
END.

FIND Customer NO-LOCK WHERE
     Customer.Custnum = mobsub.Custnum NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN "Customer not found".

IF MobSub.CLIType EQ "CONTFF" THEN lcCLIType = "CONT9".
ELSE IF MobSub.CLIType EQ "CONTSF" THEN DO:
   IF LOOKUP(MobSub.TariffBundle,"CONTSF10,CONTSF14") > 0 THEN ASSIGN
      lcCLIType = "CONTS"
      lcBundleID = "CONTS21". 
   ELSE RETURN "Unsupported tariff bundle".
END.
ELSE RETURN "Subscription type is not Fusion".

ldeSTCTS = Func.Common:mDate2TS(Func.Common:mLastDayOfMonth(TODAY)+ 1).

/* Various validations */
IF fValidateMobTypeCh(
   MobSub.Msseq,
   lcCLIType,
   ldeSTCTS,
   FALSE, /* extend contract */
   TRUE, /* bypass stc type check */
   0, /* stc order id */
   "",
   FALSE,
   OUTPUT lcError) EQ FALSE THEN RETURN lcError.

IF fValidateNewCliType(INPUT lcCLIType, INPUT lcBundleID,
                       INPUT TRUE, OUTPUT lcError) NE 0
THEN RETURN lcError.

oiRequest = fCTChangeRequest(MobSub.msseq,
                  lcCLIType,
                  lcBundleID,
                  "", /* bank account ok (since post2post)*/
                  ldeSTCTS,
                  0,  /* Credit check ok (since post2post)*/
                  0, /* extend contract 0=no extend_term_contract */
                  "" /* pcSalesman */,
                  FALSE, /* charge */
                  TRUE, /* send sms */
                  "",
                  0,
                  {&REQUEST_SOURCE_FUSION_ORDER_FALLBACK}, 
                  piOrderID,
                  0,
                  "", /*contract id*/
                  OUTPUT lcError).

IF oiRequest = 0 THEN
   RETURN "Request creation failed: " +  lcError.

RETURN "".
