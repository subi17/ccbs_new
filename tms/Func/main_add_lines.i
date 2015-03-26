&IF "{&MAIN_ADD_LINES_I}" NE "YES"
&THEN
&GLOBAL-DEFINE MAIN_ADD_LINES_I YES
/* ----------------------------------------------------------------------
  MODULE .......: main_add_lines.p
  TASK .........: Main or additional line related functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 28.08.13
  Version ......: Yoigo
----------------------------------------------------------------------- */
{commali.i}
{tmsconst.i}
{mnpoutchk.i}
{cparam2.i}
{date.i}
{fsubstermreq.i}

DEF TEMP-TABLE tt_AdditionalSIM NO-UNDO
    FIELD MsSeq    AS INT
    FIELD CustNum  AS INT
    FIELD CLI      AS CHAR.
    
FUNCTION fIsMainLineSubActive RETURNS LOGICAL
   (INPUT pcIdType AS CHAR,
    INPUT pcPersonId AS CHAR):

   DEF BUFFER Customer FOR Customer.
   DEF BUFFER Mobsub FOR Mobsub.

   FOR FIRST Customer WHERE
             Customer.Brand = gcBrand AND
             Customer.OrgId = pcPersonId AND
             Customer.CustidType = pcIdType AND
             Customer.Roles NE "inactive" NO-LOCK,
       EACH  MobSub WHERE
             MobSub.Brand   = gcBrand AND
             MobSub.InvCust = Customer.CustNum AND
             MobSub.PayType = FALSE NO-LOCK:

      IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                        CLIType.Brand = gcBrand AND
                        CLIType.CLIType = MobSub.TariffBundle AND
                        CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN 
         RETURN TRUE.
   END.

   RETURN FALSE.
END.

FUNCTION fHasPendingSTCToNonMainLine RETURNS LOGICAL
   (INPUT iiMsSeq AS INT,
    INPUT ideActStamp AS DEC):

   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER CLIType FOR CLIType.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0 AND
            MsRequest.Actstamp <= ideActStamp,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = gcBrand AND
            CLIType.CLIType = (IF MsRequest.ReqCParam5 > ""
                               THEN MsRequest.ReqCParam5
                               ELSE MsRequest.ReqCParam2):
      IF CLIType.LineType NE {&CLITYPE_LINETYPE_MAIN}
         THEN RETURN TRUE. 
   END.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0 AND
            MsRequest.Actstamp <= ideActStamp,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = gcBrand AND
            CLIType.CLIType = MsRequest.ReqCParam2:

      IF CLIType.LineType NE {&CLITYPE_LINETYPE_MAIN} THEN RETURN TRUE. 
   END.

   RETURN FALSE.
END.

FUNCTION fHasPendingRequests RETURNS LOGICAL
   (INPUT iiMsSeq AS INT,
    INPUT icCLI AS CHAR,
    INPUT iiLineType AS INT):

   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER CLIType FOR CLIType.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = gcBrand AND
            CLIType.CLIType = (IF MsRequest.ReqCParam5 > ""
                               THEN MsRequest.ReqCParam5
                               ELSE MsRequest.ReqCParam2):
      IF CLIType.LineType NE iiLineType 
         THEN RETURN TRUE. 
   END.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = gcBrand AND
            CLIType.CLIType = MsRequest.ReqCParam2:

      IF CLIType.LineType NE iiLineType THEN RETURN TRUE. 
   END.
   
   IF CAN-FIND (FIRST MsRequest WHERE
          MsRequest.MsSeq = iiMsseq AND
          MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
          LOOKUP(STRING(MsRequest.ReqStatus),
                 {&REQ_INACTIVE_STATUSES}) = 0) THEN RETURN TRUE.

   IF fIsMNPOutOngoing(INPUT icCLI) THEN RETURN TRUE.

   RETURN FALSE.

END FUNCTION.

FUNCTION fHasPendingSTCToMainLine RETURNS LOGICAL
   (INPUT iiMsSeq AS INT):
   
   DEF BUFFER CLIType FOR CLIType.
   DEF BUFFER MsRequest FOR Msrequest.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = gcBrand AND
            CLIType.CLIType = (IF MsRequest.ReqCParam5 > ""
                               THEN MsRequest.ReqCParam5
                               ELSE MsRequest.ReqCParam2):
      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} THEN RETURN TRUE. 
   END.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = gcBrand AND
            CLIType.CLIType = MsRequest.ReqCParam2:

      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} THEN RETURN TRUE. 
   END.

   RETURN FALSE.

END.

FUNCTION fIsMainLineOrderPending RETURNS LOGICAL
   (INPUT pcIdType AS CHAR,
    INPUT pcPersonId AS CHAR,
    INPUT iiExcludeOrderID AS INT):

   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderAction FOR OrderAction.
   DEF BUFFER CLIType FOR CLIType.

   FOR EACH OrderCustomer NO-LOCK WHERE   
            OrderCustomer.Brand      EQ gcBrand AND 
            OrderCustomer.CustId     EQ pcPersonId AND
            OrderCustomer.CustIdType EQ pcIdType AND
            OrderCustomer.RowType    EQ 1,
      EACH  Order NO-LOCK WHERE
            Order.Brand              EQ gcBrand AND
            Order.orderid            EQ OrderCustomer.Orderid AND
            Order.OrderType          NE {&ORDER_TYPE_RENEWAL} AND 
            Order.OrderType          NE {&ORDER_TYPE_STC} AND 
            LOOKUP(STRING(Order.statuscode),{&ORDER_INACTIVE_STATUSES}) EQ 0,
       EACH OrderAction NO-LOCK WHERE 
            OrderAction.Brand = Order.Brand AND
            OrderAction.OrderId = Order.OrderID AND
            OrderAction.ItemType = "BundleItem":

      IF iiExcludeOrderID > 0 AND Order.OrderID EQ iiExcludeOrderID THEN NEXT.
      IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                        CLIType.Brand = gcBrand AND
                        CLIType.CLIType = OrderAction.ItemKey AND
                        CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fTermAdditionalSim RETURNS LOGICAL
    (INPUT iiMsSeq       AS INT,
     INPUT icCLI         AS CHAR,
     INPUT iiCustNum     AS INT,
     INPUT iiTermReason  AS INT,
     INPUT idaTermDate   AS DATE,
     INPUT icSource      AS CHAR,
     INPUT iiOrigRequest AS INT,
     OUTPUT lvcError     AS CHAR).

   DEFINE VARIABLE lvdaSecSIMTermDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE lvdeSecSIMTermStamp AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lviMsisdnStat       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lviSimStat          AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lviQuarTime         AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lviRequest          AS INTEGER   NO-UNDO.

   IF icSource EQ {&REQUEST_SOURCE_STC} OR 
      icSource EQ {&REQUEST_SOURCE_BTC} THEN DO:
      lvdaSecSIMTermDate = fLastDayofMonth(idaTermDate).

      IF DAY(idaTermDate) <> 1 THEN
         ASSIGN lvdaSecSIMTermDate = lvdaSecSIMTermDate + 1
                lvdaSecSIMTermDate = fLastDayOfMonth(lvdaSecSIMTermDate).

      lvdeSecSIMTermStamp = fMake2Dt(lvdaSecSIMTermDate,86399).          
   END.
   ELSE IF icSource EQ {&REQUEST_SOURCE_ACC} THEN DO:
      IF DAY(idaTermDate) EQ 1 THEN 
         lvdaSecSIMTermDate = fLastDayOfMonth(idaTermDate).
      ELSE ASSIGN
         lvdaSecSIMTermDate  = ADD-INTERVAL(idaTermDate,1,"months")
         lvdaSecSIMTermDate  = fLastDayOfMonth(lvdaSecSIMTermDate).

      lvdeSecSIMTermStamp = fMake2Dt(lvdaSecSIMTermDate,86399).
   END.
   ELSE
      ASSIGN lvdaSecSIMTermDate  = ADD-INTERVAL(idaTermDate,1,"months")
             lvdaSecSIMTermDate  = fLastDayOfMonth(lvdaSecSIMTermDate)
             lvdeSecSIMTermStamp = fMake2Dt(lvdaSecSIMTermDate,86399).

   fInitialiseValues(iiTermReason,
                     fIsYoigoCLI(icCLI),
                     OUTPUT lviMsisdnStat,
                     OUTPUT lviSimStat,
                     OUTPUT lviQuarTime).

   lviRequest = fTerminationRequest(
                       iiMsSeq,
                       lvdeSecSIMTermStamp,
                       lviMsisdnStat,
                       lviSimStat,
                       lviQuarTime,
                       1, /* create fees */
                       "", /* out oper. */
                       STRING(iiTermReason),
                       icSource,
                       katun,
                       iiOrigRequest, /* orig. request */
                       OUTPUT lvcError).
   IF lviRequest EQ 0 THEN
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(iiMsSeq),
                       iiCustNum,
                       "Multi SIM termination failed",
                       lvcError).

   RETURN TRUE.

END FUNCTION.

FUNCTION fAdditionalSimTermination RETURNS LOGICAL
   (INPUT iiMsSeq   AS INTEGER,
    INPUT icSource  AS CHARACTER):
   
   DEFINE BUFFER MsRequest FOR MsRequest.     
   DEFINE BUFFER MobSub    FOR MobSub.
   DEFINE BUFFER lbMobSub  FOR MobSub.
   DEFINE BUFFER CLIType   FOR CLIType.
   
   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO.
   
   FIND FIRST MobSub NO-LOCK WHERE 
              MobSub.brand = gcBrand AND 
              MobSub.MsSeq = iiMsSeq NO-ERROR.
          
   EMPTY TEMP-TABLE tt_AdditionalSIM NO-ERROR.
          
   IF NOT CAN-FIND(
          FIRST CLIType NO-LOCK WHERE
                CLIType.Brand = gcBrand AND
                CLIType.CLIType = (IF MobSub.TariffBundle > ""
                                      THEN MobSub.TariffBundle
                                   ELSE MobSub.CLIType) AND
                CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL}) THEN RETURN FALSE.
                       
   FOR EACH lbMobSub NO-LOCK WHERE
            lbMobSub.Brand   = gcBrand        AND
            lbMobSub.InvCust = Mobsub.CustNum AND
            lbMobSub.PayType = FALSE,
      FIRST CLitype NO-LOCK WHERE
            CLitype.Brand   = gcBrand                         AND
            CLitype.CLIType = (IF lbMobsub.TariffBundle > ""
                                  THEN lbMobsub.TariffBundle
                               ELSE lbMobsub.CLIType)         AND
            CLIType.LineType > 0:
               
      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
         fHasPendingSTCToMainLine(lbMobSub.Msseq)     THEN DO:
         EMPTY TEMP-TABLE tt_AdditionalSIM NO-ERROR.
         LEAVE.
      END.

      CREATE tt_AdditionalSIM.
      ASSIGN tt_AdditionalSIM.MsSeq   = lbMobSub.MsSeq
             tt_AdditionalSIM.CustNum = lbMobSub.CustNum
             tt_AdditionalSIM.CLI     = lbMobSub.CLI.
   END.

   FOR EACH tt_AdditionalSIM NO-LOCK:
                
      IF fHasPendingRequests(lbMobSub.Msseq,
                             lbMobSub.CLI,
                             {&CLITYPE_LINETYPE_ADDITIONAL}) THEN NEXT.
                             
      fTermAdditionalSim(tt_AdditionalSIM.MsSeq,
                         tt_AdditionalSIM.CLI,
                         tt_AdditionalSIM.CustNum,
                         {&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM},
                         TODAY,
                         icSource,
                         0,
                         OUTPUT lcError).
                         
   END. /* FOR EACH ttAdditionalSIM NO-LOCK: */
   
   RETURN TRUE.
                        
END FUNCTION.

&ENDIF
