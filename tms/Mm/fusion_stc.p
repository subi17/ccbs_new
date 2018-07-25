/* ----------------------------------------------------------------------
  MODULE .......: fusion_stc.p
  TASK .........: Creates Fusion STC request
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

DEF BUFFER NewCLIType FOR CLIType.

DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR llOk AS LOG NO-UNDO. 
DEF VAR liCreditCheck AS INT NO-UNDO. 
DEF VAR lcBundleCliTypes AS CHAR NO-UNDO. 
DEF VAR ldeSTCTS AS DEC NO-UNDO. 
DEF VAR lcNewCLIType AS CHAR NO-UNDO. 
DEF VAR lcBundleID AS CHAR NO-UNDO. 
DEF VAR lcBankAcc AS CHAR NO-UNDO. 
DEF VAR llExtendContract AS LOG NO-UNDO. 
DEF VAR iiRequestFlags     AS INTEGER NO-UNDO.
DEF VAR ldaSTCDate AS DATE NO-UNDO. 
DEF VAR liSTCTime AS INT NO-UNDO. 

ASSIGN
   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = Syst.Var:gcBrand AND
           Order.Orderid = piOrderID NO-ERROR.
IF NOT AVAIL Order THEN RETURN "Order not found".
         
FIND FIRST OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand = Order.Brand AND
           OrderCustomer.OrderID = Order.OrderID AND
           OrderCustomer.RowType = 1 NO-ERROR.
IF NOT AVAIL OrderCustomer THEN RETURN "OrderCustomer not found".

IF Order.OrderType NE {&ORDER_TYPE_STC} THEN
   RETURN "Incorrect order type".

IF fIsConvergenceTariff(Order.CliType) EQ FALSE THEN
   RETURN "Order subscription type is not Fusion".

FIND mobsub NO-LOCK WHERE
     mobsub.msseq = Order.MsSeq NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN "Subscription not found".

FIND Customer NO-LOCK WHERE
     Customer.Custnum = mobsub.Custnum NO-ERROR.
IF NOT AVAILABLE Customer THEN
   RETURN "Customer not found".

/* only in prepaid->postpaid it should be allowed to change bank account*/
ASSIGN
   lcNewCLIType = Order.CLIType
   lcBankAcc = OrderCustomer.BankCode WHEN mobsub.paytype EQ TRUE 
   lcBundleID = fGetDataBundleInOrderAction(Order.OrderID,Order.CLIType)
      WHEN LOOKUP(Order.CLIType,lcBundleCLITypes) > 0.

llExtendContract = 
   CAN-FIND (FIRST OrderAction NO-LOCK WHERE
                   OrderAction.Brand    = Syst.Var:gcBrand AND
                   OrderAction.OrderId  = Order.OrderId AND
                   OrderAction.ItemType = "ExtendTermContract").

FIND FIRST OrderFusion NO-LOCK WHERE
           OrderFusion.Brand = Syst.Var:gcBrand AND
           OrderFusion.OrderID = Order.OrderId NO-ERROR.
IF NOT AVAIL OrderFusion THEN RETURN "OrderFusion not found".

IF OrderFusion.FixedInstallationTS EQ ? OR
   OrderFusion.FixedInstallationTS EQ 0 THEN
   RETURN "Missing fixed line installation date".

Func.Common:mSplitTS(OrderFusion.FixedInstallationTS,
         OUTPUT ldaSTCDate,
         OUTPUT liSTCTime).

IF ldaSTCDate < TODAY THEN ASSIGN
   ldaSTCDate = TODAY
   liSTCTime = 0.

IF fIsiSTCAllowed(MobSub.MsSeq) THEN DO:
   IF mobsub.PayType EQ FALSE THEN
      ldeSTCTS = Func.Common:mMake2DT(ldaSTCDate, 0).
   ELSE ldeSTCTS = Func.Common:mMake2DT(ldaSTCDate, liSTCTime).
END.
/* TODO: how to handle properly */
ELSE ldeSTCTS = Func.Common:mDate2TS(Func.Common:mLastDayOfMonth(TODAY) + 1).

/* Various validations */
IF fValidateMobTypeCh(
   MobSub.Msseq,
   lcNewCLIType,
   ldeSTCTS,
   llExtendContract, /* extend contract */
   TRUE, /* bypass stc type check */
   piOrderID,
   "",
   FALSE,
   OUTPUT lcError) EQ FALSE
THEN RETURN lcError.

IF fValidateNewCliType(INPUT lcNewCLIType, INPUT lcBundleID,
                       INPUT TRUE, OUTPUT lcError) NE 0
THEN RETURN lcError.

FIND FIRST NewCliType WHERE
           NewCLIType.Brand = Syst.Var:gcBrand AND
           NewCLIType.CLIType = Order.CLIType NO-LOCK.
IF NOT AVAIL NewCLIType THEN
   RETURN SUBST("Unknown CLIType &1", Order.CLIType).

IF fServAttrValue(MobSub.CLIType,
                  "TypeChg",
                  "CreditCheck",
                  OUTPUT llOk) = "0"
   OR NewCLIType.PayType = 2 THEN liCreditcheck = 0.
   
IF lcError > "" THEN RETURN lcError.
/* 0=no extend_term_contract
   1=extend_term_contract */
iiRequestFlags = IF llExtendContract THEN 1 ELSE 0.

oiRequest = fCTChangeRequest(MobSub.msseq,
                  lcNewCLIType,
                  lcBundleID,
                  lcBankAcc, /*bank code validation is already done in newton */
                  ldeSTCTS,
                  liCreditcheck,  /* 0 = Credit check ok */
                  iiRequestFlags, /* extend contract */
                  "" /* pcSalesman */,
                  FALSE, /* charge */
                  TRUE, /* send sms */
                  "",
                  0,
                  {&REQUEST_SOURCE_FUSION_ORDER}, 
                  piOrderID,
                  0,
                  "", /*contract id*/
                  OUTPUT lcError).

IF oiRequest = 0 THEN
   RETURN "Request creation failed: " +  lcError.

RETURN "".
