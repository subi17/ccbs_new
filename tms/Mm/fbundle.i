/* fbundle.i         09.11.10/aam 

*/

&IF "{&fbundle}" NE "YES" 
&THEN

&GLOBAL-DEFINE fbundle YES
   
{commali.i}
{tmsconst.i}
{date.i}
{timestamp.i}
{cparam2.i}
{fcreatereq.i}
{matrix.i}
{transname.i}
{ftaxdata.i}
{xmlfunction.i}
{date.i}
{fdss.i}
{Mm/bundle_type.i}

FUNCTION fOngoingContractAct RETURNS LOG (INPUT iiMsSeq    AS INT,
                                          INPUT icBundleId AS CHAR):
   
   DEF VAR llExist AS LOG NO-UNDO.
   
   llExist = CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                       MsRequest.MsSeq   = iiMsSeq          AND
                       MsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
                       LOOKUP(STRING(MsRequest.ReqStatus),
                              {&REQ_INACTIVE_STATUSES}) = 0 AND
                       MsRequest.ReqCParam3 = icBundleId).

   RETURN llExist.
   
END FUNCTION. /* FUNCTION fOngoingContractAct */


FUNCTION fOngoingContractTerm RETURNS LOG (INPUT iiMsSeq    AS INT,
                                           INPUT icBundleId AS CHAR):
   
   DEF VAR llExist AS LOG NO-UNDO.
   
   llExist = CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                       MsRequest.MsSeq   = iiMsSeq          AND
                       MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                       LOOKUP(STRING(MsRequest.ReqStatus),
                              {&REQ_INACTIVE_STATUSES}) = 0 AND
                       MsRequest.ReqCParam3 = icBundleId).

   RETURN llExist.
   
END FUNCTION. /* FUNCTION fOngoingContractTerm */

FUNCTION fIsBundle RETURNS LOGIC
   (icDCEvent AS CHAR):
 
   DEF VAR llBundle AS LOG  NO-UNDO.
   DEF VAR liCount  AS INT  NO-UNDO.
   
   DEF BUFFER bPerContract FOR DayCampaign.
   
   /* DUB is not considered as exchangable bundle */
   IF icDCEvent EQ "DUB" THEN RETURN FALSE.
   
   llBundle = FALSE.
   
   FOR FIRST bPerContract NO-LOCK WHERE 
             bPerContract.Brand = gcBrand AND
             bPerContract.DCEvent = icDCEvent AND
             LOOKUP(STRING(bPerContract.DCType),
                    {&PERCONTRACT_RATING_PACKAGE}) > 0:
      llBundle = TRUE.              
   END.
      
   RETURN llBundle.
    
END FUNCTION.

FUNCTION fIsBundleAllowed RETURNS LOGIC
   (icCLIType AS CHAR,
    icDCEvent AS CHAR,
    OUTPUT ocInfo AS CHAR):

   DEF VAR lcResult AS CHAR NO-UNDO. 
   
   IF NOT fIsBundle(icDCEvent) THEN DO:
      ocInfo = "Not a bundle".
      RETURN FALSE.
   END.
   
   /* is the new bundle allowed */
   IF fMatrixAnalyse(gcBrand,
                     "PERCONTR",
                     "PerContract;SubsTypeTo",
                     icDCEvent + ";" + icCLIType,
                     OUTPUT lcResult) NE 1 AND
      ENTRY(1,lcResult,";") NE "?"  
   THEN DO:
      ocInfo = "Bundle is not allowed for this subscription type".
      RETURN FALSE.
   END.
   
   RETURN TRUE.

END FUNCTION.

FUNCTION fGetActiveDataBundle RETURNS CHAR
   (iiMsSeq      AS INT,
    idActiveTime AS DEC):

   DEF VAR lcBundleList AS CHAR NO-UNDO.

   DEF VAR lcPostpaidDataBundles  AS CHAR NO-UNDO.
   DEF VAR lcPrePaidDataBundles   AS CHAR NO-UNDO.

   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER bServiceLimit  FOR ServiceLimit.

   ASSIGN lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
          lcPrePaidDataBundles  = fCParamC("PREPAID_DATA_CONTRACTS").

   /* Fetch all active bundles */
   FINDBUNDLES:
   FOR EACH bMServiceLimit NO-LOCK WHERE
            bMServiceLimit.MsSeq    = iiMsSeq AND
            bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
            bMServiceLimit.EndTS    > idActiveTime, 
      FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
            bServiceLimit.SLSeq = bMServiceLimit.SLSeq AND
     LOOKUP(bServiceLimit.GroupCode,lcPostpaidDataBundles +
            "," + lcPrePaidDataBundles) > 0:

      /* Should not return TARJD1 in active bundle list */
      IF bServiceLimit.GroupCode = "TARJD1" OR
         LOOKUP(bServiceLimit.GroupCode,lcBundleList) > 0 THEN NEXT.
      
      /* pending termination request */
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.MsSeq = iiMsSeq AND
                        MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                        LOOKUP(STRING(MsRequest.ReqStatus),
                                {&REQ_INACTIVE_STATUSES}) = 0 AND
                        MsRequest.ActStamp <= idActiveTime) THEN
         NEXT FINDBUNDLES.

      lcBundleList = lcBundleList + (IF lcBundleList > "" THEN "," ELSE "") +
                     bServiceLimit.GroupCode.
   END.

   RETURN lcBundleList.
    
END FUNCTION.

FUNCTION fGetActiveBundle RETURNS CHAR
   (iiMsSeq      AS INT,
    idActiveTime AS DEC):

   DEF VAR lcBundleList AS CHAR NO-UNDO.
   DEF VAR liLoop       AS INT NO-UNDO.
   DEF VAR licount      AS INT NO-UNDO.
   licount = NUM-ENTRIES({&REQ_ONGOING_STATUSES}).

   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER bServiceLimit  FOR ServiceLimit.
   DEF BUFFER bDayCampaign   FOR DayCampaign.

   /* Fetch all active bundles */
   FINDBUNDLES:
   FOR EACH bMServiceLimit NO-LOCK WHERE
            bMServiceLimit.MsSeq   = iiMsSeq AND
            bMServiceLimit.EndTS  >= idActiveTime, 
      FIRST bServiceLimit NO-LOCK WHERE
            bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
      FIRST bDayCampaign NO-LOCK WHERE 
            bDayCampaign.Brand   = gcBrand AND
            bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
            LOOKUP(STRING(bDayCampaign.DCType),
                   {&PERCONTRACT_RATING_PACKAGE}) > 0 AND
            STRING(bDayCampaign.DCType) <> {&DCTYPE_CUMULATIVE_RATING}:

      /* Should not return DSS in active bundle list */
      IF bServiceLimit.GroupCode BEGINS {&DSS} OR
         LOOKUP(bServiceLimit.GroupCode,lcBundleList) > 0 THEN NEXT.
      
      /* pending termination request */
/*      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.MsSeq = iiMsSeq AND
                        MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                        LOOKUP(STRING(MsRequest.ReqStatus),
                                {&REQ_INACTIVE_STATUSES}) = 0 AND
                        MsRequest.ActStamp <= idActiveTime) THEN
         NEXT FINDBUNDLES. */

      CHECK_LOOP:
      DO liLoop = 1 TO licount:
         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                           MsRequest.MsSeq = iiMsSeq AND
                           MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                           MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                           MsRequest.ReqStatus = INT(ENTRY(liLoop,({&REQ_ONGOING_STATUSES}))) AND
                           MsRequest.ActStamp <= idActiveTime) THEN
            NEXT FINDBUNDLES.
      END.

      lcBundleList = lcBundleList + (IF lcBundleList > "" THEN "," ELSE "") +
                     bServiceLimit.GroupCode.
   END.

   RETURN lcBundleList.
    
END FUNCTION.

/* YDR-800 */
FUNCTION fGetCurrentTariff RETURNS CHAR
   (BUFFER Mobsub FOR MobSub,
    OUTPUT odaActDate AS DATE):

   DEF BUFFER MServiceLimit FOR MServiceLimit.
   DEF BUFFER ServiceLimit  FOR ServiceLimit.
   DEF BUFFER MsOwner       FOR MsOwner.
   DEF BUFFER MsRequest     FOR MsRequest.

   DEF VAR lcTariffContract AS CHAR NO-UNDO.
   DEF VAR liTime           AS INT  NO-UNDO.
   DEF VAR ldeActStamp      AS DEC  NO-UNDO.
   DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.

   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").
   
   IF LOOKUP(MobSub.CLIType,lcBundleCLITypes) > 0 THEN
      lcTariffContract = MobSub.TariffBundle.

   IF lcTariffContract EQ "" THEN DO:
      FOR EACH MsOwner NO-LOCK WHERE
               MsOwner.MsSeq = MobSub.MsSeq USE-INDEX MsSeq:

         IF MsOwner.CLIType NE MobSub.CLIType THEN LEAVE.
         ldeActStamp = MsOwner.TSBegin.
      END.
      IF ldeActStamp > 0 THEN
         fSplitTS(ldeActStamp, OUTPUT odaActDate, OUTPUT liTime).

      RETURN MobSub.CLIType.
   END. /* IF lcTariffContract = "" THEN DO: */

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = Mobsub.MsSeq AND
            MsRequest.Reqtype = {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
            MsRequest.ReqCparam3 = lcTariffContract BY DoneStamp DESC:
      
      IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION}
      THEN NEXT.

      fSplitTS(MsRequest.ActStamp, OUTPUT odaActDate, OUTPUT liTime).
      RETURN MsRequest.ReqCparam3.
   END.
   
   FOR EACH MServiceLimit NO-LOCK WHERE
            MServiceLimit.MsSeq   = MobSub.MsSeq AND
            MServiceLimit.EndTS  >= fMakeTS(), 
      FIRST ServiceLimit NO-LOCK USE-INDEX SLSeq WHERE
            ServiceLimit.SLSeq = MServiceLimit.SLSeq AND
            ServiceLimit.GroupCode = lcTariffContract:
      fSplitTS(MServiceLimit.FromTS, OUTPUT odaActDate, OUTPUT liTime).
      RETURN ServiceLimit.GroupCode.
   END.
   
   RETURN "".
END.

FUNCTION fGetCONTFFraudCounterFee RETURNS DEC 
   (iiMsSeq      AS INT,
    idActiveTime AS DEC):

   DEF VAR ldaFirstDay      AS DATE NO-UNDO.
   DEF VAR ldeAmount        AS DEC  NO-UNDO.
   DEF VAR liDaysInMonth    AS INT  NO-UNDO.
   DEF VAR ldaActDate       AS DATE NO-UNDO.
   DEF VAR liTime           AS INT  NO-UNDO.

   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER bServiceLimit  FOR ServiceLimit.
   DEF BUFFER bDayCampaign   FOR DayCampaign.
   DEF BUFFER Feemodel       FOR FeeModel.
   DEF BUFFER FMItem         FOR FMItem.

   FINDBUNDLES:
   FOR EACH bMServiceLimit NO-LOCK WHERE
            bMServiceLimit.MsSeq   = iiMsSeq AND
            bMServiceLimit.EndTS  >= idActiveTime AND
            bMServiceLimit.FromTS <= idActiveTime, 
      FIRST bServiceLimit NO-LOCK WHERE
            bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
      FIRST bDayCampaign NO-LOCK WHERE 
            bDayCampaign.Brand   = gcBrand AND
            bDayCampaign.DcEvent = bServiceLimit.GroupCode AND
            bDayCampaign.DCEvent BEGINS "CONTF",
      FIRST FeeModel NO-LOCK WHERE
            FeeModel.Brand    = gcBrand AND
            FeeModel.FeeModel = bDayCampaign.FeeModel,
      FIRST FMItem NO-LOCK WHERE
            FMItem.Brand     = gcBrand AND
            FMItem.FeeModel  = FeeModel.FeeModel AND
            FMItem.FromDate <= TODAY AND
            FMItem.Todate >= TODAY:

      /* pending termination request */
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.MsSeq = iiMsSeq AND
                        MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                        LOOKUP(STRING(MsRequest.ReqStatus),
                                {&REQ_INACTIVE_STATUSES}) = 0 AND
                        MsRequest.ActStamp <= idActiveTime) THEN
         NEXT FINDBUNDLES.

      ldaFirstDay = DATE(MONTH(TODAY),1,YEAR(TODAY)).       
      fSplitTS(bMServiceLimit.FromTS, OUTPUT ldaActDate, OUTPUT liTime).

      IF ldaActDate <= ldaFirstDay THEN ldeAmount = FMItem.Amount.
      ELSE DO:
         liDaysInMonth = DAY(fLastDayOfMonth(TODAY)).
         ldeAmount = ROUND(FMItem.Amount * ((liDaysInMonth - DAY(ldaActDate) + 1) / liDaysInMonth),2).
      END.

      RETURN ldeAmount.
   END.

   RETURN 0.
    
END FUNCTION.

FUNCTION fGetTerminatedSpecificBundle RETURNS CHAR
   (iiMsSeq      AS INT,
    idActiveTime AS DEC,
    icBundleType AS CHAR):

   DEF VAR lcContracts       AS CHAR NO-UNDO. 

   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER bServiceLimit  FOR ServiceLimit.
   DEF BUFFER bDayCampaign   FOR DayCampaign.

   lcContracts = fGetBundles(icBundleType).
   IF lcContracts = "" THEN RETURN "".

   FINDBUNDLES:
   FOR EACH bMServiceLimit NO-LOCK WHERE
            bMServiceLimit.MsSeq   = iiMsSeq AND
            bMServiceLimit.FromTS <= idActiveTime, 
      FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
            bServiceLimit.SLSeq = bMServiceLimit.SLSeq AND
            LOOKUP(bServiceLimit.GroupCode,lcContracts) > 0:

      RETURN bServiceLimit.GroupCode.
   END.

   RETURN "".
    
END FUNCTION.

FUNCTION fGetCurrentBundle RETURNS CHAR
   (iiMsSeq AS INT):

   RETURN fGetActiveBundle(iiMsSeq,
                           fMakeTS()).
END FUNCTION.

FUNCTION fIsBonoVoIPAllowed RETURNS LOGICAL
   (iiMsSeq AS INT,
    ideTS AS DEC):

   DEF VAR lcActiveDataBundles    AS CHAR NO-UNDO.
   DEF VAR lcNativeVOIPTariffs AS CHAR NO-UNDO.
   DEF VAR lcNativeVoipBundles AS CHAR NO-UNDO. 
   DEF VAR i AS INT NO-UNDO. 

   DEF BUFFER MobSub  FOR MobSub.

   FIND FIRST MobSub WHERE
              MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN FALSE.

   lcNativeVOIPTariffs = fCParamC("NATIVE_VOIP_BASE_BUNDLES").
   lcNativeVoipBundles = fCParamC("NATIVE_VOIP_BUNDLES").

   /* All Native VoIP tariffs */
   IF LOOKUP(MobSub.CLIType,lcNativeVOIPTariffs) > 0 OR
      LOOKUP(MobSub.TariffBundle,lcNativeVOIPTariffs) > 0 THEN RETURN FALSE.

   /* Native VOIP bundles */
   lcActiveDataBundles = fGetActiveDataBundle(iiMsSeq,ideTS).
   DO i = 1 TO NUM-ENTRIES(lcActiveDataBundles):
      IF LOOKUP(ENTRY(i,lcActiveDataBundles), lcNativeVoipBundles) > 0 THEN RETURN FALSE.
   END.

   IF LOOKUP("BONO_VOIP",lcActiveDataBundles) > 0 OR
      fOngoingContractAct(iiMsSeq,"BONO_VOIP") THEN RETURN FALSE.

   /* Additional lines (CONTM,CONTM2) with DSS2 active */
   IF LOOKUP(MobSub.CLIType,"CONTM,CONTM2") > 0 AND
      lcActiveDataBundles = "" THEN DO:
      IF fGetActiveDSSId(MobSub.CustNum,ideTS) = "DSS2" THEN RETURN TRUE.
      ELSE RETURN FALSE.
   END.

   /* Tariffs without any data bundle (base tariff or addtional data bundle) */
   IF lcActiveDataBundles = "" THEN RETURN FALSE.

   RETURN TRUE.

END FUNCTION.

FUNCTION fIsVoIPAllowed RETURNS LOGICAL
   (iiMsSeq AS INT,
    ideTS AS DEC):

   DEF VAR lcActiveDataBundles    AS CHAR NO-UNDO.
   DEF VAR lcNativeVOIPBundles    AS CHAR NO-UNDO. 
   DEF VAR lcNativeVOIPTariffs    AS CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO. 

   DEF BUFFER MsOwner FOR MsOwner.

   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MsOwner THEN RETURN FALSE.

   lcNativeVOIPTariffs = fCParamC("NATIVE_VOIP_BASE_BUNDLES").
   lcNativeVoipBundles = fCParamC("NATIVE_VOIP_BUNDLES").

   /* All Native VoIP tariffs */
   IF LOOKUP(MsOwner.CLIType,lcNativeVOIPTariffs) > 0 OR
      LOOKUP(MsOwner.TariffBundle,lcNativeVOIPTariffs) > 0 THEN RETURN TRUE.

   lcActiveDataBundles = fGetActiveDataBundle(iiMsSeq,ideTS).
   DO i = 1 TO NUM-ENTRIES(lcActiveDataBundles):
      IF LOOKUP(ENTRY(i,lcActiveDataBundles), lcNativeVoipBundles) > 0 THEN RETURN TRUE.
   END.

   IF LOOKUP("BONO_VOIP",lcActiveDataBundles) > 0 THEN RETURN TRUE.

   RETURN FALSE.

END FUNCTION.

FUNCTION fGetDataBundleInOrderAction RETURNS CHAR
   (iiOrderId    AS INT,
    icBundleType AS CHAR):

   DEF VAR lcContracts AS CHAR NO-UNDO.

   lcContracts = fGetBundles(icBundleType).
   IF lcContracts = "" THEN RETURN "".

   FIND FIRST OrderAction WHERE
              OrderAction.Brand = gcBrand AND
              OrderAction.OrderId = iiOrderId AND
              OrderAction.ItemType = "BundleItem" AND
              LOOKUP(OrderAction.ItemKey,lcContracts) > 0
   NO-LOCK NO-ERROR.
   IF AVAIL OrderAction THEN RETURN OrderAction.ItemKey.
   
   RETURN "".

END FUNCTION.

/* used with ext.api/dextra for old CONTDx -> CONTRDx conversion */
FUNCTION fConvBundleToCLIType RETURNS CHAR
   (icDataBundle AS CHAR):

   FIND FIRST TMSCodes WHERE 
              TMSCodes.TableName = "BundleToCLIType" AND
              TMSCodes.FieldName = "CLIType" AND
              TMSCodes.CodeValue = icDataBundle
        NO-LOCK NO-ERROR.
   IF AVAILABLE TMSCodes THEN RETURN TMSCodes.ConfigValue.
   ELSE RETURN icDataBundle.

END FUNCTION.

FUNCTION fConvMFBillCodeToCLIType RETURNS CHAR
   (icMFBundleBillCode AS CHAR):

   FIND FIRST TMSCodes WHERE 
              TMSCodes.TableName = "MFCodeToCLIType" AND
              TMSCodes.FieldName = "CLIType" AND
              TMSCodes.CodeValue = icMFBundleBillCode
        NO-LOCK NO-ERROR.
   IF AVAILABLE TMSCodes THEN RETURN TMSCodes.ConfigValue.
   ELSE RETURN icMFBundleBillCode.

END FUNCTION.

FUNCTION fConvBundleToBillItem RETURNS CHAR
   (icDataBundle AS CHAR):

   FOR FIRST DayCampaign WHERE
             DayCampaign.Brand   = gcBrand AND
             DayCampaign.DCEvent = icDataBundle NO-LOCK,
       FIRST FeeModel WHERE
             FeeModel.Brand    = gcBrand AND
             FeeModel.FeeModel = DayCampaign.FeeModel NO-LOCK,
       FIRST FMItem WHERE
             FMItem.Brand     = gcBrand AND
             FMItem.FeeModel  = FeeModel.FeeModel AND
             FMItem.FromDate <= TODAY AND
             FMItem.ToDate   >= TODAY NO-LOCK:
       RETURN FMItem.BillCode.
   END.
   RETURN icDataBundle.

END FUNCTION.

FUNCTION fConvBundleToCommName RETURNS LOGICAL
   (INPUT  icBundleId       AS CHAR,
    OUTPUT ocBundleCommName AS CHAR,
    OUTPUT ocSMSSender      AS CHAR):

   FIND FIRST TMSCodes WHERE 
              TMSCodes.TableName = "BundleToCoName" AND
              TMSCodes.FieldName = "Bundle" AND
              TMSCodes.CodeValue = icBundleId
        NO-LOCK NO-ERROR.
   IF AVAILABLE TMSCodes AND TMSCodes.ConfigValue > "" THEN DO:
      ASSIGN ocBundleCommName = ENTRY(1,TMSCodes.ConfigValue,"|")
             ocSMSSender      = ENTRY(2,TMSCodes.ConfigValue,"|").
      RETURN TRUE.
   END. /* IF AVAILABLE TMSCodes AND TMSCodes.ConfigValue > "" */
   ELSE RETURN FALSE.

END FUNCTION.

FUNCTION fBundleWithSTC RETURNS LOG
   (iiMsSeq      AS INT,
    ideActStamp  AS DEC,
    ilIsVoIPNative AS LOG):

   DEF BUFFER bMsRequest FOR MsRequest.

   DEF VAR ldaReqDate    AS DATE NO-UNDO.
   DEF VAR liReqTime     AS INT  NO-UNDO.

   DEF VAR lcPostpaidDataBundles  AS CHAR NO-UNDO.
   DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.
   DEF VAR lcNativeVOIPTariffs AS CHAR NO-UNDO.
   DEF VAR lcNativeVoipBundles AS CHAR NO-UNDO. 

   fSplitTS(ideActStamp,OUTPUT ldaReqDate,OUTPUT liReqTime).

   IF liReqTime > 0 THEN
      ideActStamp = fMake2Dt(ldaReqDate + 1,0).

   ASSIGN lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
          lcDataBundleCLITypes  = fCParamC("DATA_BUNDLE_BASED_CLITYPES").

   IF ilIsVoIPNative THEN ASSIGN
      lcNativeVOIPTariffs = fCParamC("NATIVE_VOIP_BASE_BUNDLES")
      lcNativeVoipBundles = fCParamC("NATIVE_VOIP_BUNDLES").

   /* Check STC Request with data bundle */
   FIND FIRST bMsRequest NO-LOCK WHERE
              bMsRequest.MsSeq   = iiMsSeq AND
              bMsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
              LOOKUP(STRING(bMsRequest.ReqStat),"4,9,99,3") = 0 AND
              bMsRequest.ActStamp = ideActStamp USE-INDEX MsSeq NO-ERROR.
   IF AVAIL bMsRequest AND
      (LOOKUP(bMsRequest.ReqCparam2,lcDataBundleCLITypes) > 0 OR
       LOOKUP(bMsRequest.ReqCparam5,lcPostpaidDataBundles) > 0) THEN DO:
      IF lcNativeVOIPTariffs > "" AND
         (LOOKUP(bMsRequest.ReqCparam2,lcNativeVOIPTariffs) > 0 OR
          LOOKUP(bMsRequest.ReqCparam5,lcNativeVOIPTariffs) > 0) THEN
         RETURN FALSE.
      ELSE RETURN TRUE.
   END.

   /* Check BTC Request with data bundle */
   FIND FIRST bMsRequest NO-LOCK WHERE
              bMsRequest.MsSeq   = iiMsSeq AND
              bMsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
              LOOKUP(STRING(bMsRequest.ReqStat),"4,9,99,3") = 0 AND
              bMsRequest.ActStamp = ideActStamp USE-INDEX MsSeq NO-ERROR.
   IF AVAIL bMsRequest AND
      LOOKUP(bMsRequest.ReqCparam2,lcPostpaidDataBundles) > 0 THEN DO:
      IF LOOKUP(bMsRequest.ReqCparam2,lcNativeVOIPTariffs) > 0 OR
         LOOKUP(bMsRequest.ReqCparam2,lcNativeVoipBundles) > 0 THEN
         RETURN FALSE.
      ELSE RETURN TRUE.
   END.

   RETURN FALSE. 

END FUNCTION.

FUNCTION fBundleWithSTCCustomer RETURNS LOG
   (iiCustnum    AS INT,
    ideActStamp  AS DEC):

   DEF BUFFER MsRequest FOR MsRequest.

   DEF VAR ldaReqDate    AS DATE NO-UNDO.
   DEF VAR liReqTime     AS INT  NO-UNDO.

   DEF VAR lcPostpaidDataBundles  AS CHAR NO-UNDO.
   DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.

   fSplitTS(ideActStamp,OUTPUT ldaReqDate,OUTPUT liReqTime).

   IF liReqTime > 0 THEN
      ideActStamp = fMake2Dt(ldaReqDate + 1,0).

   ASSIGN lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
          lcDataBundleCLITypes  = fCParamC("DATA_BUNDLE_BASED_CLITYPES").

   /* Check STC Request with data bundle */
   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.Brand = gcBrand AND
              MsRequest.Custnum = iiCustnum AND
              MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
              LOOKUP(STRING(MsRequest.ReqStat),"4,9,99,3") = 0 AND
              MsRequest.ActStamp = ideActStamp USE-INDEX Custnum NO-ERROR.
   IF AVAIL MsRequest AND
      (LOOKUP(MsRequest.ReqCparam2,lcDataBundleCLITypes) > 0 OR
       LOOKUP(MsRequest.ReqCparam5,lcPostpaidDataBundles) > 0)
   THEN RETURN TRUE.

   /* Check BTC Request with data bundle */
   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.Brand = gcBrand AND
              MsRequest.Custnum = iiCustnum AND
              MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
              LOOKUP(STRING(MsRequest.ReqStat),"4,9,99,3") = 0 AND
              MsRequest.ActStamp = ideActStamp USE-INDEX Custnum NO-ERROR.
   IF AVAIL MsRequest AND
      LOOKUP(MsRequest.ReqCparam2,lcPostpaidDataBundles) > 0
   THEN RETURN TRUE.

   RETURN FALSE. 

END FUNCTION.

PROCEDURE pAdjustBal:

   DEFINE INPUT PARAMETER icBundle     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iiMsseq      AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER ideAdjustBal AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER ocErrorMsg  AS CHARACTER NO-UNDO.

   DEFINE VARIABLE liRequest           AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liRespCode          AS INTEGER   NO-UNDO INITIAL 9.
   DEFINE VARIABLE lcXML               AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcTaxZone           AS CHARACTER NO-UNDO.

   DEFINE BUFFER bMobSub               FOR MobSub.
   DEFINE BUFFER bCustomer             FOR Customer.

   IF ideAdjustBal <= 0 THEN RETURN.

   FIND FIRST bMobSub WHERE bMobSub.Msseq = iiMsseq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bMobSub THEN DO:
      ocErrorMsg = "MobSub record not found for subscription id: " + STRING(iiMsseq).
      RETURN.
   END. /* IF NOT AVAILABLE bMobSub THEN DO: */

   FIND FIRST bCustomer WHERE
              bCustomer.CustNum = bMobSub.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bCustomer THEN DO:
      ocErrorMsg = "Customer record not found for subscription id: " + STRING(iiMsseq).
      RETURN.
   END. /* IF NOT AVAILABLE bCustomer THEN DO: */

   lcTaxZone  = fRegionTaxZone(bCustomer.Region).
     
   DO WHILE TRUE:
      liRequest = NEXT-VALUE(PrePaidReq).
   
      IF NOT CAN-FIND(FIRST PrePaidRequest WHERE
                            PrePaidRequest.Brand     = gcBrand AND
                            PrepaidRequest.PPRequest = liRequest)
      THEN LEAVE.
   END. /* DO WHILE TRUE: */
   
   ideAdjustBal = -1 * ideAdjustBal * 100.
 
   CREATE PrePaidRequest.
   ASSIGN
      PrePaidRequest.TSRequest   = fMakeTS()
      PrePaidRequest.UserCode    = katun
      PrePaidRequest.Brand       = gcBrand
      PrePaidRequest.MsSeq       = bMobSub.MsSeq
      PrePaidRequest.CLI         = bMobSub.CLI
      PrePaidRequest.PPRequest   = liRequest
      PrePaidRequest.Request     = "AdjustmentTRequest"
      PrePaidRequest.CommLine    = "AdjustmentTRequest"
      PrePaidRequest.Source      = "CHARGE"
      PrePaidRequest.PPStatus    = 0
      PrePaidRequest.TopUpAmt    = ideAdjustBal
      /* minimum consumption is always without tax */
      PrePaidRequest.VatAmt      = 0
      PrePaidRequest.TaxZone     = lcTaxZone.
   
   IF icBundle = {&PMDUB} THEN
      PrePaidRequest.PPReqPrefix = "989".
   ELSE IF icBundle = ({&PMDUB} + "_UPSELL") THEN
      PrePaidRequest.PPReqPrefix = "988".
   ELSE IF icBundle = {&TARJ_UPSELL} THEN
      PrePaidRequest.PPReqPrefix = "976".
   ELSE IF icBundle = "TARJ7_UPSELL" THEN
      PrePaidRequest.PPReqPrefix = "975".

   RUN pp_platform.p(gcBrand,PrePaidRequest.PPRequest).
   
   lcXML = RETURN-VALUE.
      
   IF lcXML > "" AND NOT lcXML BEGINS "ERR:" THEN
      liRespCode = INT(fGetRPCNodeValue(lcXML,"responseCode")) NO-ERROR.

   ASSIGN
      PrePaidRequest.Response   = lcXML
      PrePaidRequest.RespCode   = liRespCode
      PrePaidRequest.TSResponse = fMakeTS().

   /* OK response */
   IF liRespCode <= 2 THEN DO:
      PrePaidRequest.PPStatus = 2.
 
      /* payment for adjustment */
      CREATE TopUpQueue.
      ASSIGN
         TopUpQueue.PPRequest = PrePaidRequest.PPRequest
         TopUpQueue.CLI       = PrePaidRequest.CLI
         TopUpQueue.TopUpAmt  = PrePaidRequest.TopUpAmt / 100
         TopUpQueue.VatAmt    = PrePaidRequest.VatAmt / 100
         TopUpQueue.Date      = TODAY
         TopUpQueue.Source    = PrePaidRequest.Source.
         
      CREATE Memo.
      ASSIGN
         Memo.CreStamp  = fMakeTS()
         Memo.Brand     = gcBrand
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(bMobSub.MsSeq)
         Memo.CustNum   = bMobSub.CustNum
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = katun
         Memo.MemoTitle = UPPER(icBundle) + " Balance Adjustment"
         Memo.MemoText  = "Subscription's balance has been charged with " +
            TRIM(STRING(ABS(ROUND(PrePaidRequest.TopUpAmt / 100,2)),
                        "->>>>>>>>>9.99")) +
            " euros for prepaid " + icBundle.
   END. /* IF liRespCode <= 2 THEN DO: */
   /* error occured */
   ELSE
      ASSIGN PrePaidRequest.PPStatus = 2
             ocErrorMsg = "AIR ERROR: " + STRING(PrePaidRequest.RespCode).
   
END PROCEDURE. /* PROCEDURE pAdjustBal: */

PROCEDURE pEnoughBalance:

   DEFINE INPUT PARAMETER icCLI         AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ideAmountChk  AS DECIMAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER olResult     AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE ldeCurrBal           AS DECIMAL   NO-UNDO.
  
   IF ideAmountChk > 0 THEN DO:
      RUN balancequery.p(icCLI).
      IF RETURN-VALUE > "" THEN
         ldeCurrBal = DECIMAL(RETURN-VALUE) / 100 NO-ERROR.
      IF ldeCurrBal < ideAmountChk THEN DO:
         olResult = FALSE.
         RETURN.
      END.
   END. /* IF ideAmountChk > 0 THEN DO: */
   
   olResult = TRUE.

END PROCEDURE. /* FUNCTION enough_balance RETURNS LOGICAL */


FUNCTION fCreateOrderAction RETURNS LOGICAL
   (iiOrderId AS INT,
    icType    AS CHAR,
    icKey     AS CHAR,
    icParam   AS CHAR):

   CREATE OrderAction.
   ASSIGN OrderAction.Brand = gcBrand
          OrderAction.OrderId = iiOrderId
          OrderAction.ItemType = icType
          OrderAction.ItemKey = icKey
          OrderAction.ItemParam = icParam.
   RETURN TRUE.

END.

&ENDIF
