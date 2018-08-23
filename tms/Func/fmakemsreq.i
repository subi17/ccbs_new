/* fmakemsreq.i   03.01.05/aam  

   add entries to request spool
   
   timestamp.i, fctchange.i, fctserval.i needed
   
   changes:       11.02.05/aam Salesman to fServiceRequest() and 
                               pCheckServiceLinks
                  06.10.05/aam schedule all ct-requests from web to 1st 
                               of next month
                  13.10.05/aam Salesman to fCTChangeRequest         
                  27.12.05/aam fSaldoPaymentRequest(), ReqType=2
                  10.01.06/aam fMSCustChangeRequest(), ReqType=3
                  25.01.06/aam fPendingRequest(),
                               fUserAccountRequest(),
                               address change with fMSCustChangeRequest()
                  06.02.06/aam fPerContract* 
                  13.03.06/aam new type "chglogin" to fUserAccountRequest
                  14.03.06/aam fPaymPlanRequest()
                  22.03.06/aam fMarketingRequest()
                  03.04.06/aam fPCActionRequest()
                  10.04.06/aam output parameter to pCheckServiceLinks,
                               mark created subrequests to father request
                  17.05.06/aam icParam for fServiceRequest may contain data
                               for msrequest.reqcparam3
                  22.05.06/aam update DCCLI.TermDate
                  08.12.06/mvi fSimChangeRequest (15)
                  17.01.07/aam ilCreateFees to fPCActionRequest
                  12.04.07/aam fODInvoiceRequest
                  26.04.07/aam note status 9 in pending check
                  15.05.07/aam more than one contract terminations allowed
                  29.08.07/aam common functions into fcreatereq.i
                  19.09.07/jp   fsubscriptionrequest2
                  10.10.07/jp  icBankNumber for fCTChangeRequest
                  10.10.07/jp   fsubscriptionrequest2 Removed
                  25.08.15/ilkkasav YPR-2378 fCTChangeRequest added parameter


*/   
&IF "{&fmakemsreq}" NE "YES"
&THEN

&GLOBAL-DEFINE fmakemsreq YES

{Func/fcreatereq.i}
{Func/fctserval.i}
{Func/fcustdata.i}
{Func/fixedlinefunc.i}
{Func/cparam2.i}

DEF VAR liReqCreated AS INT  NO-UNDO.

FUNCTION fActivateTARJ7Promo RETURN LOGICAL
   (INPUT iiMsSeq AS INT,
    INPUT ideActStamp AS DEC):

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrdeAction FOR OrderAction.
   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MserviceLimit FOR MserviceLimit.

   FOR EACH Order NO-LOCK WHERE
            Order.MsSeq = iiMsSeq AND
            Order.CLItype = "TARJ7" AND
            Order.OrderType <= 2 AND
            Order.StatusCode = {&ORDER_STATUS_DELIVERED},
      FIRST OrderAction NO-LOCK WHERE
            OrderAction.Brand = Syst.Var:gcBrand AND
            OrderAction.OrderId = Order.OrderId AND
            OrderAction.ItemType = "Promotion" AND
            OrderAction.ItemKey  = "TARJ7" BY Order.CRStamp DESC:
      
      IF ideActStamp >= 20150201 THEN DO:

         IF Order.OrderType EQ 2 AND 
            ideActStamp >= 20150301 THEN RETURN FALSE.
         ELSE IF Order.OrderType < 2 AND
            ideActStamp >= 20150216 THEN RETURN FALSE.
                       
         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                           MsRequest.MsSeq = iiMsSeq AND
                           MsRequest.ReqType = 1 AND
                           MsRequest.ReqCParam1 EQ "SHAPER" AND
                           MsRequest.ReqCParam2 EQ "TARJ7_PROMO") THEN
            RETURN FALSE.
      END.

      RETURN TRUE.
   END.

   RETURN FALSE.
END.

FUNCTION fChkTiming RETURNS CHARACTER
   (icOldCLIType AS CHAR,
    icNewCLIType AS CHAR,
    idtActDate   AS DATE).

   DEF VAR lcTiming   AS CHAR NO-UNDO. 
   DEF VAR llAllowChg AS LOG  NO-UNDO. 
   
   /* day can not be set into past (time is not so relevant) */
   IF idtActDate < TODAY - 31 THEN RETURN 
      "Change day cannot be too far in the past".
   
   /* if timing is set for 1. of month then no need to check */
   IF DAY(idtActDate) = 1 THEN RETURN "".

   /* can change take place only on 1. of month 
      i.e. old type can only be closed on last day of month */ 
   lcTiming = fServAttrValue(icOldCLIType,
                             "TypeChg",
                             "OnLastDay",
                             OUTPUT llAllowChg).
               
   /* if old clitype demands timing to 1. of month then 
      other days are not allowed */
   IF lcTiming = "1"
   THEN RETURN "Should be scheduled to 1st day of next month".
   ELSE RETURN "".

END FUNCTION.

FUNCTION fCreateMsRequestParam RETURNS LOGICAL
   (INPUT iiMsRequest  AS INT,
    INPUT icParamName  AS CHAR,
    INPUT icParamType  AS CHAR,
    INPUT icParamValue AS CHAR):

   IF NOT CAN-FIND(FIRST MsRequestParam NO-LOCK WHERE
                         MsRequestParam.MsRequest EQ iiMsRequest  AND
                         MsRequestParam.ParamName EQ icParamName) THEN DO:
      CREATE MsRequestParam.
      ASSIGN MsRequestParam.MsRequest = iiMsRequest
             MsRequestParam.ParamName = icParamName
             MsRequestParam.ParamType = icParamType.

      CASE icParamType:
         WHEN {&CHARVAL} THEN MsRequestParam.CharValue = icParamValue.
         WHEN {&INTVAL}  THEN MsRequestParam.IntValue  = INT(icParamValue).
         WHEN {&DECVAL}  THEN MsRequestParam.DecValue  = DEC(icParamValue).
         WHEN {&DATEVAL} THEN MsRequestParam.DateValue = DATE(icParamValue).
      END CASE.

      RETURN TRUE.

   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckMsRequestParam RETURNS LOGICAL
   (INPUT iiMsRequest   AS INT,
    INPUT icParamName   AS CHAR,
    OUTPUT ocParamValue AS CHAR):

   DEF BUFFER MsRequestParam FOR MsRequestParam.

   FIND FIRST MsRequestParam NO-LOCK WHERE
              MsRequestParam.MsRequest EQ iiMsRequest AND
              MsRequestParam.ParamName EQ icParamName NO-ERROR.

   IF NOT AVAIL MsRequestParam THEN
      RETURN FALSE.

   CASE MsRequestParam.ParamType:
      WHEN {&CHARVAL} THEN ocParamValue = MsRequestParam.CharValue.
      WHEN {&INTVAL}  THEN ocParamValue = STRING(MsRequestParam.IntValue).
      WHEN {&DECVAL}  THEN ocParamValue = STRING(MsRequestParam.DecValue).
      WHEN {&DATEVAL} THEN ocParamValue = STRING(MsRequestParam.DateValue).
   END CASE.

   RETURN TRUE.

END FUNCTION.

/* CLI type change */
FUNCTION fCTChangeRequest RETURNS INTEGER
   (INPUT  iiMsSeq        AS INT,
    INPUT  icNewType      AS CHAR,
    INPUT  icBundleType   AS CHAR,
    INPUT  icBankNumber   AS CHAR,
    INPUT  idChgStamp     AS DEC,
    INPUT  iiCreditCheck  AS INT,
    INPUT  iiRequestFlags AS INTEGER,
    INPUT  icSalesman     AS CHAR,
    INPUT  ilCreateFees   AS LOG,
    INPUT  ilSendSMS      AS LOG,
    INPUT  icCreator      AS CHAR,   /* who made the request */
    INPUT  ideFee         AS DEC,
    INPUT  icSource       AS CHAR,
    INPUT  iiOrderID      AS INT,
    INPUT  iiOrigReq      AS INT,    /* Father request id */
    INPUT  icDMSInfo      AS CHAR,   /* For DMS usage contract_id, channel */
    OUTPUT ocResult       AS CHAR).

   DEF VAR llCRes        AS LOG  NO-UNDO.
   DEF VAR lcCReqTime    AS CHAR NO-UNDO.
   DEF VAR liCReqTime    AS INT  NO-UNDO.
   DEF VAR ldtCReqDate   AS DATE NO-UNDO.
   DEF VAR llProCustomer AS LOGI NO-UNDO.

   ocResult = fChkRequest(iiMsSeq,
                          0,
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   /* check if there are scheduling rules */
   IF idChgStamp = ? THEN DO:

      ASSIGN ldtCReqDate = TODAY
             liCReqTime  = TIME.

      /* all web requests are scheduled to 1st of next month */
      IF Syst.Var:katun = "WEB" THEN lcCReqTime = "1".
      
      /* should old type be closed only on last of month 
         -> set request date as the 1st of next month */
      ELSE lcCReqTime = fChkTiming(bReqSub.CLIType,
                                   "",
                                   ldtCReqDate).

      IF lcCReqTime > "" THEN DO:
         IF MONTH(ldtCReqDate) = 12 
         THEN ldtCReqDate = DATE(1,1,YEAR(ldtCReqDate) + 1).
         ELSE ldtCReqDate = DATE(MONTH(ldtCReqDate) + 1,1,YEAR(ldtCReqDate)).
         
         liCReqTime = 18000.
      END.
       
      /* is there a time limit for scheduling (on old type) */
      IF DAY(ldtCReqDate) = 1 THEN DO:
         lcCReqTime = fServAttrValue(bReqSub.CLIType,
                                      "TypeChg",
                                      "SchedTime",
                                      OUTPUT llCRes). 
         IF lcCReqTime NE ? AND lcCReqTime > "" 
         THEN liCReqTime = INTEGER(ENTRY(1,lcCReqTime,".")) * 3600 + 
                           INTEGER(ENTRY(2,lcCReqTime,".")) * 60   -
                           600.
      END. 
      
      idChgStamp = Func.Common:mMake2DT(ldtCReqDate,
                            liCReqTime).
    
   END. 
   
   /* double check (duplicate RPC call) */
   ocResult = fChkRequest(iiMsSeq,
                          0,
                          "",
                          icCreator).
   IF ocResult > "" THEN RETURN 0.                       
  
   /* PRO */
   IF iiOrderId > 0 THEN DO:
      FIND FIRST Order NO-LOCK WHERE
                 Order.brand EQ "1" AND
                 Order.orderid EQ iiOrderId NO-ERROR.
      IF AVAIL Order AND INDEX(order.orderchannel,"pro") > 0 THEN DO:
         llProCustomer = TRUE.
      END.
      ELSE
         llProCustomer = FALSE.
   END.
   ELSE
      llProCustomer = fIsProSubscription(iiMsSeq).

   fCreateRequest(0,
                  Func.Common:mMakeTS(),
                  icCreator,
                  ilCreateFees,
                  ilSendSMS).
   /* YDR-2038 
      exempt penalty fee when doing an STC
      iiRequestFlags (0=no extend_term_contract, 
                  1=extend_term_contract
                  2=exclude_term_penalty)
      */
   ASSIGN bCreaReq.ReqCParam1  = bReqSub.CLIType
          bCreaReq.ReqCParam2  = icNewType
          bCreaReq.ReqCparam3  = icBankNumber
          bCreaReq.ReqCparam5  = icBundleType
          bCreaReq.ReqCparam6  = icDMSInfo
          bCreaReq.ReqDParam1  = idChgStamp
          bCreaReq.ReqDParam2  = ideFee
          bCreaReq.ReqIParam1  = iiCreditCheck
          bCreaReq.ReqIParam2  = iiOrderID
          bCreaReq.ReqIParam4  = (IF llProCustomer OR icSource EQ {&REQUEST_SOURCE_MIGRATION} THEN 1 ELSE 0) /* This is for request action rules, so only bundles specific to PRO customer are activated*/
          bCreaReq.ReqIParam5  = iiRequestFlags
          bCreaReq.Salesman    = icSalesman
          bCreaReq.ReqSource   = icSource
          bCreaReq.OrigRequest = iiOrigReq
          liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.

   /* initial actions */
   RUN Mm/requestaction_init.p (liReqCreated).

   /* Send right away SMS related to the CLI Type change */
   RUN Mm/requestaction_sms.p(INPUT liReqCreated,
                              INPUT icNewType,
                              INPUT icSource).

   RETURN liReqCreated.
             
END FUNCTION.

FUNCTION fServiceActStamp RETURNS DECIMAL
   (iiMsSeq   AS INT,
    icServCom AS CHAR,
    iiValue   AS INT).

   DEF VAR llSerClose  AS LOG  NO-UNDO.
   DEF VAR ldChgStamp  AS DEC  NO-UNDO. 
   DEF VAR liCReqTime  AS INT  NO-UNDO.
   DEF VAR ldtCReqDate AS DATE NO-UNDO.

   DEF BUFFER bReqComp  FOR ServCom.
   DEF BUFFER bReqSer   FOR SubSer.

   /* default is current time */
   ASSIGN ldtCReqDate = TODAY
          liCReqTime  = TIME
          llSerClose  = FALSE.

   /* check if there are scheduling rules for closing service */
   IF iiValue = 0 THEN DO:   
      FIND bReqComp NO-LOCK WHERE
           bReqComp.Brand   = Syst.Var:gcBrand AND
           bReqComp.ServCom = icServCom NO-ERROR.
      IF AVAILABLE bReqComp AND bReqComp.CloseTime = 1  
      THEN llSerClose = TRUE.
   END.
      
   /* otherwise check links to other components; some of them may have
      a rule for closing */
   ELSE FOR EACH ScUpdRule NO-LOCK WHERE
                 ScUpdRule.Brand    = Syst.Var:gcBrand   AND
                 ScUpdRule.ServCom  = icServCom AND
                 ScUpdRule.OldValue = 0         AND
                 ScUpdRule.NewValue = 1,
           FIRST bReqComp NO-LOCK WHERE
                 bReqComp.Brand   = Syst.Var:gcBrand AND
                 bReqComp.ServCom = ScUpdRule.UpdServCom:

      IF bReqComp.CloseTime = 1 THEN DO:
         
         /* other component is active and must be closed */
         FIND FIRST bReqSer NO-LOCK WHERE
                    bReqSer.MsSeq   = iiMsSeq AND
                    bReqSer.ServCom = bReqComp.ServCom NO-ERROR.
         IF AVAILABLE bReqSer AND bReqSer.SSStat > 0 THEN DO:
            llSerClose = TRUE.
            LEAVE.
         END. 
      END. 
   END.

   /* should service be closed only on last of month 
      -> set request date as the 1st of next month */
   IF llSerClose THEN DO:
      IF DAY(ldtCReqDate) NE 1 THEN DO:
         IF MONTH(ldtCReqDate) = 12 
         THEN ldtCReqDate = DATE(1,1,YEAR(ldtCReqDate) + 1).
          ELSE ldtCReqDate = DATE(MONTH(ldtCReqDate) + 1,1,YEAR(ldtCReqDate)).
      END.
      liCReqTime = 60.
   END.
       
   ldChgStamp = Func.Common:mMake2DT(ldtCReqDate,
                         liCReqTime).
   
   IF ldChgStamp = ? THEN ldChgStamp = 0.
   
   RETURN ldChgStamp.
     
END FUNCTION.


/* activate / close a service (component) */
FUNCTION fServiceRequest RETURNS INTEGER
   (INPUT  iiMsSeq      AS INT,
    INPUT  icServCom    AS CHAR,
    INPUT  iiValue      AS INT,
    INPUT  icParam      AS CHAR,
    INPUT  idChgStamp   AS DEC,
    INPUT  icSalesman   AS CHAR,
    INPUT  ilCreateFees AS LOG,
    INPUT  ilSendSMS    AS LOG,
    INPUT  icCreator    AS CHAR,   /* who made the request */
    INPUT  icSource     AS CHAR,
    INPUT  iiOrigRequest AS INT,
    INPUT  ilMandatory  AS LOG,
    OUTPUT ocResult     AS CHAR).

   DEF VAR liOldValue AS INT  NO-UNDO.
   DEF VAR lcChildren AS CHAR NO-UNDO.
   DEF VAR liThisReq  AS INT  NO-UNDO.
   
   IF NOT (icServCom EQ "LP" AND
      icParam EQ "remove" AND
      icSource EQ {&REQUEST_SOURCE_BARRING}) THEN DO:

      ocResult = fChkRequest(iiMsSeq,
                             1,
                             icServCom,
                             icCreator).
      IF ocResult > "" THEN RETURN 0.                       
   END.

   /* set activation time if caller has not determined it */
   IF idChgStamp = ? OR idChgStamp = 0 THEN DO:
    
      idChgStamp = fServiceActStamp(iiMsSeq,
                                    icServCom,
                                    iiValue).
   END. 

   fCreateRequest(1,
                  idChgStamp,
                  icCreator,
                  ilCreateFees,
                  ilSendSMS).

   ASSIGN bCreaReq.ReqCParam1 = icServCom
          bCreaReq.ReqIParam1 = iiValue
          bCreaReq.ReqCParam2 = icParam
          bCreaReq.Salesman   = icSalesman
          bCreaReq.ReqSource  = icSource
          bCreaReq.OrigReq    = iiOrigRequest
          bCreaReq.Mandatory  = INTEGER(ilMandatory)
          liThisReq           = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   /* links to other components */
   IF CAN-FIND(FIRST ScUpdRule WHERE
                     ScUpdRule.Brand   = Syst.Var:gcBrand AND
                     ScUpdRule.ServCom = icServCom) AND
      LOOKUP(Syst.Var:katun,"WEB,NEWTON") > 0
   THEN DO:
   
      liOldValue = IF iiValue > 0 THEN 0 ELSE 1. 
      
      RUN pCheckServiceLinks (iiMsSeq,
                              icServCom,
                              liOldValue,
                              iiValue,
                              idChgStamp,
                              icSalesman,
                              icCreator,
                              icSource,
                              iiOrigRequest,
                              ilMandatory,
                              OUTPUT lcChildren).
                              
      IF lcChildren > "" THEN DO:
         FIND bCreaReq WHERE bCreaReq.MsRequest = liThisReq EXCLUSIVE-LOCK.
         bCreaReq.ReqCParam4 = lcChildren.
         RELEASE bCreaReq.
      END.
   END.

   RETURN liThisReq.
             
END FUNCTION.
 
/* change the value of a service attribute */
FUNCTION fServAttrRequest RETURNS INTEGER
   (INPUT  iiMsSeq      AS INT,
    INPUT  icServCom    AS CHAR,
    INPUT  icServAttr   AS CHAR,
    INPUT  icValue      AS CHAR,
    INPUT  idChgStamp   AS DEC,
    INPUT  ilCreateFees AS LOG,
    INPUT  ilSendSMS    AS LOG,
    INPUT  icCreator    AS CHAR,   /* who made the request */
    INPUT  icSource     AS CHAR,
    INPUT  iiOrigRequest AS INT,
    INPUT  ilMandatory  AS LOG,
    OUTPUT ocResult     AS CHAR).

   ocResult = fChkRequest(iiMsSeq,
                          1,
                          icServCom + "." + icServAttr,
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   fCreateRequest(1,
                  idChgStamp,
                  icCreator,
                  ilCreateFees,
                  ilSendSMS).

   ASSIGN bCreaReq.ReqCParam1 = icServCom + "." + icServAttr
          bCreaReq.ReqCParam2 = icValue
          bCreaReq.ReqSource  = icSource
          bCreaReq.OrigReq    = iiOrigRequest
          bCreaReq.Mandatory  = INTEGER(ilMandatory)
          liReqCreated        = bCreaReq.MsRequest.

   RELEASE bCreaReq.
 
   RETURN liReqCreated.
             
END FUNCTION.

/* saldo epayment / egift -> create a fatime event and open subscription */
FUNCTION fSaldoPaymentRequest RETURNS INTEGER
   (INPUT  iiMsSeq      AS INT,
    INPUT  icType       AS CHAR,  /* saldo / egift */
    INPUT  idAmount     AS DEC,
    INPUT  idChgStamp   AS DEC,
    INPUT  ilCreateFees AS LOG,
    INPUT  ilSendSMS    AS LOG,
    INPUT  icCreator    AS CHAR,   /* who made the request */
    OUTPUT ocResult     AS CHAR).

   IF LOOKUP(icType,"saldo,egift") = 0 THEN DO:
      ocResult = "Unknown payment type".
      RETURN 0.
   END. 
   
   ocResult = fChkRequest(iiMsSeq,
                          2,
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   /* set activation time if caller has not determined it */
   IF idChgStamp = ? THEN idChgStamp = Func.Common:mMakeTS().

   fCreateRequest(2,
                  idChgStamp,
                  icCreator,
                  ilCreateFees,
                  ilSendSMS).

   ASSIGN bCreaReq.ReqCParam1 = icType
          bCreaReq.ReqDParam1 = idAmount
          liReqCreated        = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
             
END FUNCTION.

FUNCTION fAddressRequest RETURNS INTEGER
   (INPUT  iiCustnum      AS INT,    /* subscription         */
    INPUT  idActStamp     AS DEC,
    INPUT  icAddress      AS CHAR,
    INPUT  icZip          AS CHAR,
    INPUT  icCity         AS CHAR,
    INPUT  icRegion       AS CHAR,
    INPUT  icCountry      AS CHAR,
    INPUT  icCOName       AS CHAR, 
    INPUT  icStreetCode   AS CHAR,  
    INPUT  icCityCode     AS CHAR,   
    INPUT  icTownCode     AS CHAR,   
    INPUT  icSource       AS CHAR,   /* source of request */
    INPUT  iiOrigReq      AS INT,    /* father request */
    OUTPUT ocResult       AS CHAR):

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF BUFFER blMobsub FOR Mobsub.
   DEF BUFFER bACC FOR MsRequest.
   DEF BUFFER Customer FOR Customer.
  
   DEFINE VARIABLE iiZip AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iiRegion AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iiStreetCode AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iiCityCode AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iiTownCode AS INTEGER NO-UNDO. 
   
   DEFINE VARIABLE lcInvGroup AS CHARACTER NO-UNDO.

   FIND FIRST Customer WHERE Customer.Custnum = iiCustnum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN DO:
      ocResult = SUBST("Customer for &1 not found", iiCustNum).
      RETURN 0.
   END.

   iiZip = INT(icZip) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN ocResult = "ZipCode must be numeral".
   IF ocResult > "" THEN RETURN 0.
   
   iiRegion = INT(icRegion) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN ocResult = "Region must be numeral".
   IF ocResult > "" THEN RETURN 0.
   
   iiStreetCode = INT(icStreetCode) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN ocResult = "StreetCode must be numeral".
   IF ocResult > "" THEN RETURN 0.
   
   iiCityCode = INT(icCityCode) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN ocResult = "CityCode must be numeral".
   IF ocResult > "" THEN RETURN 0.
   
   iiTownCode = INT(icTownCode) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN ocResult = "TownCode must be numeral".
   IF ocResult > "" THEN RETURN 0.

   /* YOT-4089 Zipcode/region validation done in Newton
      so some special cases need to pass TMS validation. */
   IF icSource NE {&REQUEST_SOURCE_NEWTON} THEN DO:
      IF icZip > "" AND SUBSTRING(icZip,1,2) NE icRegion THEN DO:
         ocResult =  "There is a conflict between zipcode and region".
         RETURN 0.
      END.
   END.

   IF icRegion ne "00" THEN 
      FIND FIRST Region WHERE Region.Region = icRegion NO-LOCK NO-ERROR.
   IF NOT AVAIL Region THEN DO:
      ocResult = "Unknown region " + icRegion.
      RETURN 0.
   END.
   
   lcInvGroup  = fDefInvGroup(icRegion).
   
   FIND InvGroup WHERE
        InvGroup.Brand    = Syst.Var:gcBrand AND
        InvGroup.InvGroup = lcInvGroup NO-LOCK NO-ERROR.
   FIND Region WHERE Region.Region = icRegion NO-LOCK NO-ERROR.
   
   IF AVAILABLE Region AND AVAILABLE InvGroup AND
      Region.TaxZone NE InvGroup.TaxZone THEN DO:
      ocResult = "There is a conflict between Invoice Group's and Region's"
             + " tax zone definitions.".
      RETURN 0.
   END.

   /* Check ongoing address change requests */
   FIND FIRST MsRequest WHERE
      MsRequest.Brand = "1" AND
      MsRequest.Reqtype = 6 AND
      MsRequest.CustNum = iiCustnum AND
      LOOKUP(STRING(MsRequest.ReqStatus),"0,1,3") >  0 
   NO-LOCK NO-ERROR.

   IF AVAIL MsRequest THEN DO:
      ocResult = "Request creation not allowed due to business rules".
      RETURN 0.
   END.

   IF icRegion ne "00" AND NOT AVAIL Region THEN DO:
      ocResult = SUBST("Unknown region &1",icRegion).
      RETURN 0.
   END.

   FOR EACH blMobSub NO-LOCK WHERE
            blMobSub.Brand   = Syst.Var:gcBrand AND
            blMobSub.AgrCust = iiCustNum,
      FIRST bACC NO-LOCK WHERE
            bACC.MsSeq   = blMobSub.MsSeq AND
            bACC.ReqType = 10           AND
            LOOKUP(STRING(bACC.ReqStatus),"2,4,9") = 0:
      ocResult = "Pending ACC on subscription " + STRING(blMobSub.MsSeq).
      RETURN 0.
   END.

   IF ocResult > "" THEN RETURN 0.            

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest(6,
                  idActStamp,
                  "",
                  FALSE,    /* create fees */
                  FALSE).   /* sms */
   ASSIGN
      bCreaReq.CustNum     = iiCustnum
      bCreaReq.ReqCParam1  = icAddress
      bCreaReq.ReqCParam2  = icCity
      bCreaReq.ReqCParam3  = icCountry
      bCreaReq.ReqCParam4  = icCOName
      bCreaReq.ReqIParam1  = iiZip
      bCreaReq.ReqIParam2  = iiRegion
      bCreaReq.ReqIParam3  = iiStreetCode
      bCreaReq.ReqIParam4  = iiCityCode
      bCreaReq.ReqDParam1  = iiTownCode
      bCreaReq.ReqSource   = icSource
      bCreaReq.OrigReq     = iiOrigReq
      liReqCreated         = bCreaReq.MsRequest.
 
   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

/* changes to user account */
FUNCTION fUserAccountRequest RETURNS INTEGER
   (INPUT  iiCustNum  AS INT,    /* whose account is changed */
    INPUT  icChgType  AS CHAR,   /* passwd, new, loginpwd, chglogin */
    INPUT  icCreator  AS CHAR,   /* who made the request */
    OUTPUT ocResult   AS CHAR).

   DEF VAR ldChgStamp AS DEC NO-UNDO.
   
   IF iiCustNum = 0 OR 
      NOT CAN-FIND(Customer WHERE Customer.CustNum = iiCustNum)
   THEN DO:
      ocResult = "Invalid customer nbr".
      RETURN 0.
   END. 

   IF icChgType = "" THEN icChgType = "passwd".
   
   IF LOOKUP(icChgType,"passwd,new,loginpwd,chglogin") = 0 THEN DO:
      ocResult = "Invalid change type".
      RETURN 0.
   END.
   
   ocResult = fChkRequest(iiCustNum,
                          5,
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   /* activation time is always immediately */
   ldChgStamp = Func.Common:mMakeTS().

   fCreateRequest(5,
                  ldChgStamp,
                  icCreator,
                  FALSE,     /* create fees */
                  FALSE).    /* send SMS    */

   ASSIGN bCreaReq.CustNum    = iiCustNum
          bCreaReq.ReqCParam1 = icChgType
          liReqCreated        = bCreaReq.MsRequest.
   
   RELEASE bCreaReq.
   
   RETURN liReqCreated.
             
END FUNCTION.

/* order a pin for activating periodical contract */
FUNCTION fPerContractPIN RETURNS INTEGER
   (INPUT  iiCustNum   AS INT,    /* customer to whom PIN is sent   */
    INPUT  icCreator   AS CHAR,   /* who made the request           */
    INPUT  idActStamp  AS DEC,    /* when request should be handled */
    OUTPUT ocResult    AS CHAR).

   IF iiCustNum = 0 OR 
      NOT CAN-FIND(Customer WHERE Customer.CustNum = iiCustNum)
   THEN DO:
      ocResult = "Invalid customer nbr".
      RETURN 0.
   END. 

   ocResult = fChkRequest(iiCustNum,
                          7,
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   /* activation time */
   IF idActStamp = ? OR idActStamp = 0
   THEN idActStamp = Func.Common:mMakeTS().

   fCreateRequest(7,
                  idActStamp,
                  icCreator,
                  FALSE,     /* create fees */
                  FALSE).    /* send SMS    */

   ASSIGN bCreaReq.CustNum    = iiCustNum
          liReqCreated        = bCreaReq.MsRequest.
   
   RELEASE bCreaReq.
   
   RETURN liReqCreated.
             
END FUNCTION.



/* update periodical contract */
FUNCTION fPCUpdateRequest RETURNS INTEGER
   (INPUT  iiMsSeq      AS INT,    /* subscription         */
    INPUT  icContrType  AS CHAR,   /* daycampaign.dcevent  */
    INPUT  idActStamp   AS DEC,    /* when request should be handled */
    INPUT  icFields     AS CHAR,   /* fields to be updated */
    INPUT  icValues     AS CHAR,   /* values in those fields */
    INPUT  ilCreateFees AS LOG,    /* fees */
    INPUT  icSource     AS CHAR,   /* where created */
    INPUT  icCreator    AS CHAR,   /* who made the request */
    OUTPUT ocResult     AS CHAR).

   DEF VAR liOrigReq  AS INT  NO-UNDO.
 
   /* validate mobsub */
   IF iiMsSeq = 0 OR 
      (NOT CAN-FIND(MobSub WHERE MobSub.MsSeq = iiMsSeq))
   THEN DO:
      ocResult = "Invalid subscription ID".
      RETURN 0.
   END. 
   /* validate contrat type */
   IF NOT CAN-FIND(FIRST DayCampaign WHERE 
                         DayCampaign.Brand   = Syst.Var:gcBrand AND
                         DayCampaign.DCEvent = icContrType)
   THEN DO:
      ocResult = "Invalid contract type".
      RETURN 0.
   END.

   IF icCreator BEGINS "OrigReq:" THEN ASSIGN
      liOrigReq = INTEGER(ENTRY(2,icCreator,":"))
      icCreator = "".

   /* other request running */   
   ocResult = fChkRequest(iiMsSeq,
                          8,
                          icContrType,
                          icCreator).
   IF ocResult > "" THEN RETURN 0. 
   /* activation time */
   IF idActStamp = ? OR idActStamp = 0
   THEN idActStamp = Func.Common:mMakeTS().

   fCreateRequest(8,
                  idActStamp,
                  icCreator,
                  FALSE,     /* create fees */
                  FALSE).    /* send SMS    */

   ASSIGN bCreaReq.ReqSource   = icSource
          bCreaReq.ReqCParam1  = icFields
          bCreaReq.ReqCParam2  = "update"
          bCreaReq.ReqCParam3  = icContrType
          bCreaReq.ReqCParam4  = icValues
          bCreaReq.CreateFees  = ilCreateFees
          bCreaReq.OrigRequest = liOrigReq
          bCreaReq.SendSMS     = 0         /* never send SMS */
          liReqCreated         = bCreaReq.MsRequest.
   
   RELEASE bCreaReq.
   
   RETURN liReqCreated.
             
END FUNCTION.

/* activate/terminate a periodical contract */
FUNCTION fPCActionRequest RETURNS INTEGER
   (INPUT  iiMsSeq        AS INT,    /* subscription         */
    INPUT  icContrType    AS CHAR,   /* daycampaign.dcevent  */
    INPUT  icActType      AS CHAR,   /* act,term,canc,iterm,cont       */
    INPUT  idActStamp     AS DEC,    /* when request should be handled */
    INPUT  ilCreateFees   AS LOG,    /* fees */
    INPUT  icSource       AS CHAR,   /* where created */
    INPUT  icCreator      AS CHAR,   /* who made the request */
    INPUT  iiOrigRequest  AS INT,    /* main request */
    INPUT  ilMandatory    AS LOG,    /* main request waits for this */
    INPUT  icSMS          AS CHAR,   /* sms */
    INPUT  ideResidualFee AS DEC,    /* payterm residual fee */
    INPUT  iiPerContID    AS INT,    /* Periodical Contract-ID */
    INPUT  icSVAParams    AS CHAR,   /* Parameters to be stoerd for SVA vase*/
    OUTPUT ocResult       AS CHAR):

   DEF VAR liReqType  AS INT  NO-UNDO.
   DEF VAR lcSource   AS CHAR NO-UNDO.
   DEF VAR liWaitFor  AS INT  NO-UNDO.
   DEF VAR lcActParam AS CHAR NO-UNDO.
   DEF VAR lcBONOContracts  AS CHAR NO-UNDO.
   
   DEF BUFFER bMsRequest FOR MsRequest.
   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER bReqOwner FOR MsOwner.

   lcBONOContracts = fCParamC("BONO_CONTRACTS").
   
   IF NUM-ENTRIES(icActType,":") > 1 THEN DO:
      ASSIGN 
         lcActParam = ENTRY(2,icActType,":")
         icActType  = ENTRY(1,icActType,":").
         
      IF lcActParam BEGINS "wait" THEN
         liWaitFor = INTEGER(SUBSTRING(lcActParam,5)) NO-ERROR.
   END.
  
   CASE icActType:
   WHEN "act"  OR
   WHEN "cont" OR
   WHEN "reactivate" OR
   WHEN "recreate" THEN liReqType = {&REQTYPE_CONTRACT_ACTIVATION}. /*8 - activate/continue/re-create service */
   WHEN "term" OR 
   WHEN "canc" OR
   WHEN "iterm" THEN liReqType = {&REQTYPE_CONTRACT_TERMINATION}.     /*9 - cancel/terminate service */
   OTHERWISE DO:
      ocResult = "Invalid action".
      RETURN 0.
   END.   
   END CASE. 

   IF iiMsSeq = 0 OR 
      (liReqType NE 9 AND NOT CAN-FIND(MobSub WHERE MobSub.MsSeq = iiMsSeq))
   THEN DO:
      ocResult = "Invalid subscription ID".
      RETURN 0.
   END. 

   IF NOT CAN-FIND(FIRST DayCampaign WHERE 
                         DayCampaign.Brand   = Syst.Var:gcBrand AND
                         DayCampaign.DCEvent = icContrType)
   THEN DO:
      ocResult = "Invalid contract type".
      RETURN 0.
   END.
   
   ocResult = fChkRequest(iiMsSeq,
                          liReqType,
                          icContrType,
                          IF icSource = {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} 
                             THEN {&SKIP_MsRequest}
                          ELSE icSource).

   IF ocResult > "" THEN RETURN 0.            
  
   /* activation time */
   IF idActStamp = ? OR idActStamp = 0 THEN
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest(liReqtype,
                  idActStamp,
                  icCreator,
                  FALSE,     /* create fees */
                  FALSE).    /* send SMS    */

   IF liReqType = 9 AND NOT AVAILABLE bReqSub THEN DO:
      FIND FIRST bReqOwner WHERE bReqOwner.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bReqOwner THEN DO:
         DELETE bCreaReq.
         ocResult = "Subscription not found".
         RETURN 0.
      END.

      ASSIGN 
          bCreaReq.MsSeq      = bReqOwner.MsSeq
          bCreaReq.CLI        = bReqOwner.CLI
          bCreaReq.CustNum    = bReqOwner.CustNum.
    END.
   /*NOTE: rReqCparam4 is reserved in this case  for ontractid
           instead of using the commonc ReqCparam6 for that purpose*/
   ASSIGN bCreaReq.ReqSource   = icSource
          bCreaReq.ReqCParam2  = icActType
          bCreaReq.ReqCParam3  = icContrType
          bCreaReq.ReqCparam6  = icSVAParams
          bCreaReq.ReqIParam3  = iiPerContID 
          bCreaReq.ReqIParam2  = liWaitFor
          bCreaReq.ReqDParam2  = ideResidualFee
          bCreaReq.CreateFees  = ilCreateFees
          bCreaReq.OrigRequest = iiOrigRequest
          bCreaReq.Mandatory   = INTEGER(ilMandatory)
          bCreaReq.SendSMS     = int((icSMS > ""))
          bCreaReq.SMSText     = icSMS
          liReqCreated         = bCreaReq.MsRequest.

   IF liReqType EQ 8 AND icContrType EQ "TARJ7" THEN DO:
      IF fActivateTARJ7Promo(iiMsSeq, idActStamp) THEN
         bCreaReq.ReqDParam1 = 1228.
   END.

   IF liReqType EQ 8 AND 
      LOOKUP(icContrType,"TARJ7,TARJ9") > 0 AND
      CAN-FIND(FIRST Order NO-LOCK WHERE
                     Order.MsSeq = iiMsSeq AND
                     Order.CLIType = icContrType AND
                     Order.Crstamp >= 20170301 AND
                     Order.Crstamp < 20170405.25200 AND
                     Order.OrderType < 2) THEN DO:
      
      FIND ServiceLimit NO-LOCK where
           ServiceLimit.groupcode = icContrType and
           ServiceLimit.dialtype = {&DIAL_TYPE_GPRS} no-error.
      IF AVAIL ServiceLimit THEN     
         bCreaReq.ReqDParam1 = ServiceLimit.inclamt * 2.
   END.
   
   /* Landing Page Enhancement with Upgrade UPSELL */
   IF bCreaReq.ReqCParam3 = "UPGRADE_UPSELL" OR
      bCreaReq.ReqCParam3 = "DSS_UPSELL_UPGRADE" THEN DO:
      FIND FIRST bMsRequest WHERE
                 bMsRequest.MsRequest = bCreaReq.OrigRequest
           NO-LOCK NO-ERROR.
      IF AVAILABLE bMsRequest THEN DO:
         bCreaReq.ReqCParam5 = bMsRequest.ReqCParam5. /* Fee Model */
         FIND FIRST TMSCodes WHERE
                    TMSCodes.TableName = "UpsellUpgrade" AND
                    TMSCodes.FieldName = "BundleLimit"   AND
                    TMSCodes.CodeValue = bMsRequest.ReqCParam5
              NO-LOCK NO-ERROR.
         IF AVAILABLE TMSCodes THEN
            bCreaReq.ReqDParam1 = DEC(TMSCodes.ConfigValue). /* Upgrade Upsell Limit */
      END. /* IF AVAILABLE bMsRequest THEN DO: */
   END. /* IF bCreaReq.ReqCParam3 = "UPGRADE_UPSELL" */

   RELEASE bCreaReq.

   /* Send right away SMS related to the bundle (de)activation */
   IF LOOKUP(icContrType,lcBONOContracts + ",TARJ_UPSELL") > 0 THEN DO:
      FIND FIRST bReqOwner WHERE bReqOwner.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF AVAILABLE bReqOwner THEN
         RUN Mm/requestaction_sms.p(INPUT liReqCreated,
                                 INPUT bReqOwner.CLIType,
                                 INPUT icSource).
   END. /* IF liReqType = 9 AND */
   
   RETURN liReqCreated.
             
END FUNCTION.

/* create a payment plan */
FUNCTION fPaymPlanRequest RETURNS INTEGER
   (INPUT  iiInvNum      AS INT,
    INPUT  iiCustNum     AS INT,
    INPUT  icPlanType    AS CHAR,    /* duedate / partpaym / paymplan */
    INPUT  icBatches     AS CHAR,    /* duedate1;amount1;duedate2;amount2;..*/
    INPUT  ilCreateFees  AS LOG,
    INPUT  icCreator     AS CHAR,
    OUTPUT ocResult      AS CHAR).

   DEF VAR ldActStamp AS DEC NO-UNDO.
   
   ocResult = fChkRequest(iiCustNum,
                          11,
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   IF LOOKUP(icPlanType,"duedate,partpaym,paymplan") = 0 THEN DO:
      ocResult = "Invalid action".
      RETURN 0.
   END.
   
   /* set activation time */
   ldActStamp = Func.Common:mMakeTS().

   fCreateRequest(11,
                  ldActStamp,
                  icCreator,
                  ilCreateFees,
                  FALSE        /* send SMS */).

   ASSIGN bCreaReq.ReqIParam1 = iiInvNum
          bCreaReq.CustNum    = iiCustNum
          bCreaReq.ReqCParam1 = icPlanType
          bCreaReq.ReqCParam2 = icBatches
          liReqCreated        = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

FUNCTION fMarketingRequest RETURNS INTEGER     
   (INPUT  iiCustNum  AS INT,    /* customer   */
    INPUT  icDirMark  AS CHAR,   /* direct marketing:  sms;email;post */
    INPUT  icOutMark  AS CHAR,   /* 3rd party marketing:  sms;email;post */
    INPUT  icCreator  AS CHAR,   /* who made the request           */
    INPUT  idActStamp AS DEC,    /* when request should be handled */
    OUTPUT ocResult   AS CHAR).

   /* dirmark and outmark should contain either "true" or "1" for 
      allowed values, e.g. "true;true;false" or "1;1;0"
   */
   
   IF iiCustNum = 0 OR 
      NOT CAN-FIND(Customer WHERE Customer.CustNum = iiCustNum)
   THEN DO:
      ocResult = "Invalid customer nbr".
      RETURN 0.
   END. 

   /* both values are mandatory */
   IF NUM-ENTRIES(icDirMark,";") < 3 OR 
      NUM-ENTRIES(icOutMark,";") < 3 THEN DO:
      ocResult = "New values have not been given".
      RETURN 0.
   END.
   
   ocResult = fChkRequest(iiCustNum,
                          12,
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   /* activation time */
   IF idActStamp = ? OR idActStamp = 0
   THEN idActStamp = Func.Common:mMakeTS().

   fCreateRequest(12,
                  idActStamp,
                  icCreator,
                  FALSE,     /* create fees */
                  FALSE).    /* send SMS    */

   ASSIGN bCreaReq.CustNum    = iiCustNum
          bCreaReq.ReqCParam1 = icDirMark
          bCreaReq.ReqCParam2 = icOutMark
          liReqCreated        = bCreaReq.MsRequest.
   
   RELEASE bCreaReq.
   
   RETURN liReqCreated.
             
END FUNCTION.


/* check links to other components */
PROCEDURE pCheckServiceLinks:

   DEF INPUT  PARAMETER iiMsSeq     AS INT  NO-UNDO.      
   DEF INPUT  PARAMETER icServCom   AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER iiOldValue  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiNewValue  AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER idLinkStamp AS DEC  NO-UNDO.
   DEF INPUT  PARAMETER icSalesman  AS CHAR NO-UNDO. 
   DEF INPUT  PARAMETER icCreator   AS CHAR NO-UNDO. 
   DEF INPUT  PARAMETER icSource    AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER iiOrigRequest AS INT NO-UNDO.
   DEF INPUT  PARAMETER ilMandatory AS LOG  NO-UNDO.
   DEF OUTPUT PARAMETER ocCreated   AS CHAR NO-UNDO.
   
   DEF VAR lcRespChar  AS CHAR NO-UNDO.
   DEF VAR liCreated   AS INT  NO-UNDO.
   
   DEF BUFFER bLinkSer FOR SubSer.
   
   IF iiOldValue = iiNewValue THEN RETURN.
   
   /* e.g. saldo-components may have a value > 1 */ 
   ASSIGN iiOldValue  = MIN(iiOldValue,1)
          iiNewValue  = MIN(iiNewValue,1)
          ocCreated   = "".
          
   IF idLinkStamp = 0 THEN idLinkStamp = Func.Common:mMakeTS().
 
   FOR EACH ScUpdRule NO-LOCK WHERE
            ScUpdRule.Brand    = Syst.Var:gcBrand    AND
            ScUpdRule.ServCom  = icServCom  AND
            ScUpdRule.OldValue = iiOldValue AND
            ScUpdRule.NewValue = iiNewValue:

      /* little sanity check */
      IF ScUpdRule.ServCom = ScUpdRule.UpdServCom OR
         ScUpdRule.UpdServCom = "" THEN NEXT.
         
      FIND FIRST bLinkSer NO-LOCK WHERE
                 bLinkSer.MsSeq   = iiMsSeq AND
                 bLinkSer.ServCom = ScUpdRule.UpdServcom NO-ERROR.

      IF AVAIL bLinkSer THEN DO:

         /* unhandled request */
         FIND FIRST bCreaReq NO-LOCK WHERE
                    bCreaReq.MsSeq      = bLinkSer.MsSeq   AND
                    bCreaReq.ReqType    = 1                AND
                    bCreaReq.ReqCParam1 = bLinkSer.ServCom AND
                    bCreaReq.ReqStatus  = 0 NO-ERROR.
                   
         IF (AVAILABLE bCreaReq AND
             bCreaReq.ReqIParam1 NE ScUpdRule.UpdValue)   OR
            (NOT AVAILABLE bCreaReq AND
             bLinkSer.SSStat NE ScUpdRule.UpdValue)
         THEN DO:
         
            /* cancel pending request which conflicts with this rule */
            IF AVAILABLE bCreaReq THEN DO:
               FIND CURRENT bCreaReq EXCLUSIVE-LOCK.
               ASSIGN bCreaReq.ReqStatus = 4
                      bCreaReq.DoneStamp = idLinkStamp
                      bCreaReq.Memo      = "Service update rule".
            END.
            
            /* make a change request (cron run handles it) */
            liCreated = fServiceRequest(iiMsSeq,
                                        bLinkSer.ServCom,
                                        ScUpdRule.UpdValue,
                                        bLinkSer.SSParam,
                                        idLinkStamp,
                                        icSalesman,
                                        TRUE,      /* fees */
                                        FALSE,     /* sms */          
                                        icCreator, 
                                        icSource,
                                        iiOrigRequest,
                                        ilMandatory,
                                        OUTPUT lcRespChar).
                                        
            IF liCreated > 0 THEN
               ocCreated = ocCreated + (IF ocCreated > "" THEN "," ELSE "") +
                           STRING(liCreated).
         END.
      END. 
   END.  
  
END PROCEDURE.

FUNCTION fIsOrderForProCustomer RETURNS LOGICAL
   (INPUT iiOrderID AS INTEGER):

   DEF BUFFER Order FOR Order.

   FIND FIRST Order NO-LOCK WHERE Order.Brand = Syst.Var:gcBrand AND Order.OrderID = iiOrderID NO-ERROR.

   IF INDEX(Order.orderchannel,"PRO") > 0 THEN
       RETURN TRUE.
   ELSE 
       RETURN FALSE.
   
END FUNCTION.

FUNCTION fSubscriptionRequest RETURNS INTEGER
   (INPUT  iiMSSeq       AS INT,    /* MSSeq */
    INPUT  icCli         AS CHAR,
    INPUT  iiCustNum     AS INT,
    INPUT  iiTrafficType AS INT,
    INPUT  icCreator     AS CHAR,
    INPUT  idActStamp    AS DEC,    /* when request should be handled */
    INPUT  icReqParam    AS CHAR,  /*Type of operation*/ 
    INPUT  icReqParam2   AS CHAR,  /*Order / New SIM or ISDN*/
    INPUT  icReqParam3   AS CHAR,  /*DMS usage, old SIM*/
    INPUT  icReqParam4   AS CHAR,  /*DMS usage, reason*/
    INPUT  icReqParam6   AS CHAR,  /*DMS usage, contract_id*/
    INPUT  ilCreateFees  AS LOG,
    INPUT  ideCharge     AS DEC,
    INPUT  icReqSource   AS CHAR,
    OUTPUT ocResult      AS CHAR).

   DEFINE VARIABLE liReqType     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcSpecial     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llProCustomer AS LOGICAL   NO-UNDO INIT FALSE.
   DEFINE VARIABLE liOrderId     AS INTEGER   NO-UNDO.

   IF (icReqParam EQ "CREATE" OR
      icReqParam EQ "CREATE-FIXED") AND
      (iiMSSeq  = 0 OR
       NOT CAN-FIND(Order WHERE
                    Order.MSSeq = iiMSSeq)) THEN DO:
      ocResult = "Invalid Order subscription".
      RETURN 0.
   END.

   CASE icReqParam:

      WHEN "CREATE" THEN liReqType = {&REQTYPE_SUBSCRIPTION_CREATE}. /*13*/
      
      WHEN "CREATE-FIXED" THEN liReqType = {&REQTYPE_FIXED_LINE_CREATE}.  /*14*/

      WHEN "CHANGEICC" THEN liReqType = {&REQTYPE_ICC_CHANGE}. /*15*/

      WHEN "CHANGEMSISDN" THEN liReqType = {&REQTYPE_MSISDN_CHANGE}. /*19*/

      OTHERWISE DO:
         ocResult = SUBST("Invalid request type: &1", icReqParam).
         RETURN 0.
      END.

   END.

   ocResult = fChkRequest(iiMsSeq,
                          liReqType,
                          "",
                          icCreator).

   IF (ocResult NE "" AND ocResult NE ?) THEN RETURN 0.

   /* activation time */
   IF idActStamp = ? OR idActStamp = 0 THEN
      idActStamp = Func.Common:mMakeTS().

   IF liReqType = {&REQTYPE_SUBSCRIPTION_CREATE} THEN 
   DO:  
       ASSIGN
           liOrderId     = INT(icReqParam2) 
           llProCustomer = fIsOrderForProCustomer(liOrderId).
   END.
     
   fCreateRequest(liReqType,
                  idActStamp,
                  icCreator,
                  ilCreateFees, /* create fees */
                  FALSE).       /* send SMS    */

   ASSIGN
      bCreaReq.MsSeq      = iiMSSeq
      bCreaReq.CLI        = icCLI
      bCreaReq.CustNum    = iiCustNum
      bCreaReq.ReqCParam1 = icReqParam
      bCreaReq.ReqCParam2 = icReqParam2
      bCreaReq.ReqCParam3 = icReqParam3
      bCreaReq.ReqCParam4 = icReqParam4
      bCreaReq.ReqCParam6 = icReqParam6
      bCreaReq.ReqIParam1 = iiTrafficType
      bCreaReq.ReqIParam4 = (IF llProCustomer THEN 1 ELSE 0) /* This is for request action rules, so only bundles specific to PRO customer are activated */
      bCreaReq.ReqDParam2 = ideCharge
      bCreaReq.ReqSource  = icReqSource
      liReqCreated        = bCreaReq.MsRequest.

   RELEASE bCreaReq.

   RETURN liReqCreated.

END FUNCTION.

/* create an on demand invoice */
FUNCTION fODInvoiceRequest RETURNS INTEGER
   (INPUT  iiCustNum     AS INT,    /* invoice customer */
    INPUT  iiMsSeq       AS INT,
    INPUT  idActStamp    AS DEC,    /* when request should be handled */
    INPUT  ilCreateFees  AS LOG,
    INPUT  ilSendSMS     AS LOG,
    INPUT  icCreator     AS CHAR,
    OUTPUT ocResult      AS CHAR).

   ocResult = fChkRequest(iiMsSeq,
                          20,
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest(20,
                  idActStamp,
                  icCreator,
                  ilCreateFees,
                  ilSendSMS).

   ASSIGN 
      bCreaReq.MsSeq   = iiMsSeq
      bCreaReq.CustNum = iiCustNum
      liReqCreated     = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

/* create sub-request */ 

FUNCTION fSubRequest RETURNS INTEGER
    (INPUT  iiMSSeq       AS INT,    /* MSSeq */
     INPUT  icCli         AS CHAR,
     INPUT  iiCustNum     AS INT,
     INPUT  ilCreateFees  AS LOG,
     INPUT  ilSendSMS     AS LOG,
     INPUT  icCreator     AS CHAR,
     INPUT  idActStamp    AS DEC,    /* when request should be handled */
     INPUT  icReqParam    AS CHAR,
     INPUT  icReqParam2   AS CHAR,
     INPUT  iiReqType     AS INT,
     INPUT  iiOrigRequest AS INT,
     INPUT  iiMandatory   AS INT,
     OUTPUT ocResult      AS CHAR).
   
   ocResult = fChkRequest(iiMsSeq,
                          iiReqType,
                          "",
                          icCreator).
    
   IF ocResult > "" THEN RETURN 0.
   
   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest(iiReqtype,
                  idActStamp,
                  icCreator,
                  ilCreateFees,
                  ilSendSMS).

   ASSIGN 
      bCreaReq.MsSeq       = iiMsSeq
      bCreaReq.CustNum     = iiCustNum
      liReqCreated         = bCreaReq.MsRequest
      bCreaReq.ReqCparam1  = icReqParam
      bCreaReq.ReqCparam2  = icReqParam2
      bCreaReq.OrigRequest = iiOrigRequest
      bCreaReq.Mandatory   = iiMandatory.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

FUNCTION fAfterSalesRequest RETURNS INTEGER
    (INPUT  iiMSSeq       AS INT,    /* MSSeq */
     INPUT  iiOrderId     AS INT,
     INPUT  icCreator     AS CHAR,
     INPUT  idActStamp    AS DEC,    /* when request should be handled */
     INPUT  icReqSource   AS CHAR,
     OUTPUT ocResult      AS CHAR).
   
   ocResult = fChkRequest(iiMsSeq,
                          46,
                          "",
                          icCreator).
    
   IF ocResult > "" THEN RETURN 0.
   
   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest(46,
                  idActStamp,
                  icCreator,
                  FALSE,
                  FALSE). /* send sms */

   ASSIGN 
      liReqCreated         = bCreaReq.MsRequest
      bCreaReq.ReqIparam1  = iiOrderId
      bCreaReq.ReqSource   = icReqSource.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

FUNCTION fChargeCompRequest RETURNS INTEGER
   (INPUT idActStamp     AS DEC,    /* when request should be handled */
    INPUT icUserCode     AS CHAR,   /* user code */
    INPUT iiMsSeq        AS INT,   /* MobSub  sequence */
    INPUT icCLI          AS CHAR,  /* mobsub CLI */ 
    INPUT iiCustNum      AS INT,   /* customer number */
    INPUT idCharge       AS DEC,   /* Charge or compensation value */
    INPUT icServFeeKey   AS CHAR,
    INPUT iiOrigRequest AS INT,
    INPUT icSource AS CHAR):
 
   DEF VAR liReqCreated AS INT NO-UNDO.

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest({&REQTYPE_CHARGE_AND_COMPENSATION},
                  idActStamp,
                  icUserCode,
                  TRUE,    /* create fees */
                  FALSE).   /* sms */


   ASSIGN
      bCreaReq.CLI          = icCLI 
      bCreaReq.MsSeq        = iiMsSeq 
      bCreaReq.CustNum      = iiCustNum
      bCreaReq.ReqCParam1   = icServFeeKey 
      bCreaReq.ReqDParam1   = idCharge
      bCreaReq.ReqSource = icSource
      bCreaReq.OrigRequest = iiOrigRequest
      liReqCreated         = bCreaReq.MsRequest.
 
   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

FUNCTION fEmailSendingRequest RETURNS INTEGER
   (INPUT idActStamp      AS DEC,    /* when request should be handled */
    INPUT icUserCode      AS CHAR,   /* user code */
    INPUT iiCustnum       AS INT,
    INPUT icCLI           AS CHAR,   /* mobsub CLI */
    INPUT icEmailAddress  AS CHAR,   /* Email Address */
    INPUT icEmailTemplate AS CHAR,   /* Email Template */
    INPUT icEmailToken    AS CHAR,   /* Email Token */
    INPUT iiIntParam1     AS INT,    /* integer parameter */
    INPUT icSource        AS CHAR,
    OUTPUT ocResult       AS CHAR):

   DEF VAR liReqCreated AS INT NO-UNDO.

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest({&REQTYPE_EMAIL_SENDING},
                  idActStamp,
                  icUserCode,
                  FALSE,    /* create fees */
                  FALSE).   /* sms         */

   ASSIGN
      bCreaReq.CLI        = icCLI
      bCreaReq.Custnum    = iiCustnum
      bCreaReq.ReqCparam1 = icEmailTemplate
      bCreaReq.ReqCparam2 = icEmailAddress
      bCreaReq.ReqCparam3 = icEmailToken
      bCreaReq.ReqSource  = icSource
      bCreaReq.ReqIParam1 = iiIntParam1
      liReqCreated        = bCreaReq.MsRequest.

   RELEASE bCreaReq.

   RETURN liReqCreated.

END FUNCTION. /* FUNCTION fEmailSendingRequest */

FUNCTION fRevertRenewalOrderRequest RETURNS INTEGER
    (INPUT  iiMSSeq       AS INT,    /* MSSeq */
     INPUT  iiOrderId     AS INT,
     INPUT  icCreator     AS CHAR,
     INPUT  idActStamp    AS DEC,    /* when request should be handled */
     INPUT  icReqSource   AS CHAR,
     OUTPUT ocResult      AS CHAR).
   
   ocResult = fChkRequest(iiMsSeq,
                          {&REQTYPE_REVERT_RENEWAL_ORDER},
                          "",
                          icCreator).
    
   IF ocResult > "" THEN RETURN 0.
   
   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest({&REQTYPE_REVERT_RENEWAL_ORDER},
                  idActStamp,
                  icCreator,
                  FALSE,
                  FALSE). /* send sms */

   ASSIGN 
      liReqCreated         = bCreaReq.MsRequest
      bCreaReq.ReqIparam1  = iiOrderId
      bCreaReq.ReqSource   = icReqSource.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

FUNCTION fInstallmentChangeRequest RETURNS INTEGER
   (INPUT iiMsSeq        AS INT,    /* subscription */
    INPUT icOldContract  AS CHAR,   /* old (current) contract */ 
    INPUT icNewContract  AS CHAR,   /* new contract */
    INPUT icSource       AS CHAR,
    INPUT icCreator      AS CHAR,  
    INPUT iiOrigRequest  AS INT,    /* main request */
    INPUT ilMandatory    AS LOG,    /* is subrequest mandatory */
    INPUT iiPerContID    AS INT,
    INPUT ideResidualFee AS DEC,
    OUTPUT ocResult      AS CHAR):
 
   DEF VAR liReqCreated    AS INT  NO-UNDO.

   DEF BUFFER bMobSub   FOR MobSub.

   FIND FIRST bMobSub WHERE
              bMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL bMobSub THEN DO:
      ocResult = "MobSub not found".
      RETURN 0.
   END. /* IF NOT AVAIL bMobSub THEN DO: */

   ocResult = fChkRequest(iiMsSeq,
                          {&REQTYPE_INSTALLMENT_CONTRACT_CHANGE},
                          icOldContract,
                          icCreator).
   IF ocResult > "" THEN RETURN 0.

   fCreateRequest({&REQTYPE_INSTALLMENT_CONTRACT_CHANGE},
                  Func.Common:mMakeTS(),
                  icCreator,
                  TRUE, 
                  FALSE).   /* sms */

   ASSIGN
      bCreaReq.MsSeq       = iiMsSeq
      bCreaReq.ReqIParam3  = iiPerContID
      bCreaReq.ReqCParam1  = icOldContract
      bCreaReq.ReqCParam2  = icNewContract
      bCreaReq.ReqDParam2  = ideResidualFee
      bCreaReq.ReqSource   = icSource
      bCreaReq.OrigRequest = iiOrigRequest
      bCreaReq.Mandatory   = INTEGER(ilMandatory)
      liReqCreated         = bCreaReq.MsRequest.
 
   RELEASE bCreaReq.

   RETURN liReqCreated.
     
END FUNCTION.

FUNCTION fConvFixedSTCReq RETURNS INTEGER
   (INPUT icCLIType   AS CHAR,
    INPUT iiMsSeq     AS INT,
    INPUT ideActStamp AS DEC,
    INPUT icReqSource AS CHAR,
    INPUT iiMsRequest AS INT):

   DEF VAR liRequest AS INT  NO-UNDO.
   DEF VAR lcError   AS CHAR NO-UNDO.
   DEF VAR lcResult  AS CHAR NO-UNDO.

   IF fListMatrix(Syst.Var:gcBrand,
                  "CONVFIXEDSTC",
                  "SubsTypeFrom;SubsTypeTo",
                  icCLIType,
                  OUTPUT lcResult) = 1 THEN DO:

      liRequest = fCTChangeRequest(iiMsSeq,
                                   lcResult,
                                   "",    /* lcBundleID */
                                   "",    /* lcBankAcc = bank code validation is already done in newton */
                                   ideActStamp,
                                   0,     /* liCreditcheck 0 = Credit check ok */
                                   0,     /* extend contract 0=no extend_term_contract */
                                   ""     /* pcSalesman */,
                                   FALSE, /* charge */
                                   TRUE,  /* send sms */
                                   "",
                                   0,
                                   icReqSource,
                                   0,     /* piOrderID */
                                   iiMsRequest,
                                   "",    /*contract_id*/
                                   OUTPUT lcError).

      IF liRequest = 0 THEN
         Func.Common:mWriteMemo("MobSub",
                          STRING(iiMsSeq),
                          0,
                          "STC to " + lcResult + "failed",
                          lcError).
   END.

   RETURN liRequest.

END FUNCTION.

FUNCTION fCustomerCategoryChangeRequest RETURNS INTEGER
   (INPUT idActStamp      AS DECIMAL   ,    /* when request should be handled */
    INPUT icUserCode      AS CHARACTER ,   /* user code */
    INPUT iiMSSeq         AS INTEGER   ,
    INPUT iiCustnum       AS INTEGER   ,
    INPUT icNewCategory   AS CHARACTER ,     
    INPUT icOldCategory   AS CHARACTER ,
    INPUT icCLI           AS CHARACTER ,   /* mobsub CLI */
    INPUT icSource        AS CHARACTER ,
    OUTPUT ocResult       AS CHARACTER ):

   DEF VAR liReqCreated AS INT NO-UNDO.

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN
      idActStamp = Func.Common:mMakeTS().

   fCreateRequest({&REQTYPE_CATEGORY_CHG},
                  idActStamp,
                  icUserCode,
                  FALSE,    /* create fees */
                  FALSE).   /* sms         */

   ASSIGN
      bCreaReq.CLI        = icCLI
      bCreaReq.MsSeq      = iiMSSeq
      bCreaReq.Custnum    = iiCustnum
      bCreaReq.ReqCparam1 = icNewCategory
      bCreaReq.ReqCparam2 = icOldCategory
      bCreaReq.ReqSource  = icSource
      liReqCreated        = bCreaReq.MsRequest.

   RELEASE bCreaReq.

   RETURN liReqCreated.

END FUNCTION. /* FUNCTION fCustomerCategoryChangeRequest*/

&ENDIF            
 
