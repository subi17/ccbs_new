/* fbundle.i         09.11.10/aam 

*/

&IF "{&fbundle}" NE "YES" 
&THEN

&GLOBAL-DEFINE fbundle YES
   
{Syst/tmsconst.i}
{Func/matrix.i}
{Func/ftaxdata.i}
{Func/xmlfunction.i}
{Func/fdss.i}
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
 
   DEF VAR llBundle            AS LOG  NO-UNDO.
   DEF VAR liCount             AS INT  NO-UNDO.
   DEF VAR lcPROFlexUpsellList AS CHAR NO-UNDO.

   DEF BUFFER bPerContract FOR DayCampaign.
   
   /* DUB is not considered as exchangable bundle */
   IF icDCEvent EQ "DUB" THEN RETURN FALSE.
   
   ASSIGN
      llBundle            = FALSE
      lcPROFlexUpsellList = fCParamC("PRO_FLEX_UPSELL_LIST").
   
   FOR FIRST bPerContract NO-LOCK WHERE 
             bPerContract.Brand = Syst.Var:gcBrand AND
             bPerContract.DCEvent = icDCEvent AND
             ((LOOKUP(STRING(bPerContract.DCType), {&PERCONTRACT_RATING_PACKAGE}) > 0) OR 
              bPerContract.BundleTarget = {&TELEVISION_BUNDLE} OR
              bPerContract.BundleTarget = {&DC_BUNDLE_TARGET_SVA} OR
              (LOOKUP(icDCEvent, lcPROFlexUpsellList) > 0)): 
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
      ocInfo = "Not a bundle" + icDCEvent.
      RETURN FALSE.
   END.
   
   /* is the new bundle allowed */
   IF fMatrixAnalyse(Syst.Var:gcBrand,
                     "PERCONTR",
                     "PerContract;SubsTypeTo",
                     icDCEvent + ";" + icCLIType,
                     OUTPUT lcResult) NE 1 AND
      ENTRY(1,lcResult,";") NE "?"  
   THEN DO:
      ocInfo = "Bundle is not allowed for this subscription type " + icDCEvent + " " + icCLIType.
      RETURN FALSE.
   END.
   
   RETURN TRUE.

END FUNCTION.

FUNCTION fGetAllowedBundlesForSubscriptionType RETURNS CHAR
  (icCliType AS CHAR):
  
  DEF VAR liCount             AS INT  NO-UNDO.
  DEF VAR lcCliType           AS CHAR NO-UNDO.
  DEF VAR lcSubsTypePrefix    AS CHAR NO-UNDO.
  DEF VAR lcAllowedBundleList AS CHAR NO-UNDO.

  DEFINE BUFFER bf_MxItem FOR MxItem.
  DEFINE BUFFER bf_Matrix FOR Matrix.

  ASSIGN 
    lcSubsTypePrefix = (IF icCliType BEGINS "CONTDSL" THEN
                            "CONTDSL*,CONT*"
                        ELSE IF icCliType BEGINS "CONTFH" THEN
                            "CONT*"
                        ELSE IF icCliType BEGINS "CONT" THEN
                            "CONT*"
                        ELSE IF icCliType BEGINS "TARJ" THEN
                            "TARJ*"
                        ELSE "")
    lcSubsTypePrefix = lcSubsTypePrefix + (IF lcSubsTypePrefix <> "" THEN "," ELSE "") + icCliType.

  IF lcSubsTypePrefix > "" THEN
  DO liCount = 1 TO NUM-ENTRIES(lcSubsTypePrefix):
      FOR EACH bf_Matrix WHERE bf_Matrix.Brand = Syst.Var:gcBrand AND bf_Matrix.MXKey = "PERCONTR" NO-LOCK By bf_Matrix.Prior:

          IF bf_Matrix.MXRes <> 1 THEN
              NEXT.

          ASSIGN lcCliType = ENTRY(liCount,lcSubsTypePrefix).

          FOR EACH bf_MxItem WHERE bf_MxItem.MxSeq = bf_Matrix.MxSeq AND bf_MxItem.MxName = "SubsTypeTo" AND bf_MxItem.MxValue = lcCliType NO-LOCK:             
              FOR EACH MxItem WHERE MxItem.MxSeq = bf_MxItem.MxSeq AND MxItem.MXName = "PerContract" NO-LOCK:

                  FIND FIRST DayCampaign WHERE Daycampaign.Brand = Syst.Var:gcBrand AND Daycampaign.DCEvent = MxItem.MxValue NO-LOCK NO-ERROR.
                  IF AVAIL DayCampaign AND LOOKUP(DayCampaign.DcType, {&PERCONTRACT_RATING_PACKAGE} + ",6") > 0 AND LOOKUP(DayCampaign.DCEvent, lcAllowedBundleList) = 0 THEN
                      ASSIGN lcAllowedBundleList = lcAllowedBundleList + (IF lcAllowedBundleList <> "" THEN "," ELSE "") + DayCampaign.DCEvent.

              END.
          END.
      END.
  END.

  RETURN lcAllowedBundleList.

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

   DEF VAR lcBundleList        AS CHAR NO-UNDO.
   DEF VAR liLoop              AS INT  NO-UNDO.
   DEF VAR licount             AS INT  NO-UNDO.

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
            bDayCampaign.Brand   = Syst.Var:gcBrand AND
            bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
            LOOKUP(STRING(bDayCampaign.DCType),{&PERCONTRACT_RATING_PACKAGE}) > 0 AND
            LOOKUP(bDayCampaign.DCEvent,"MM_DATA600") = 0 AND
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
         Func.Common:mSplitTS(ldeActStamp, OUTPUT odaActDate, OUTPUT liTime).

      RETURN MobSub.CLIType.
   END. /* IF lcTariffContract = "" THEN DO: */

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = Mobsub.MsSeq AND
            MsRequest.Reqtype = {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
            MsRequest.ReqCparam3 = lcTariffContract BY DoneStamp DESC:
      
      IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION}
      THEN NEXT.

      Func.Common:mSplitTS(MsRequest.ActStamp, OUTPUT odaActDate, OUTPUT liTime).
      RETURN MsRequest.ReqCparam3.
   END.
   
   FOR EACH MServiceLimit NO-LOCK WHERE
            MServiceLimit.MsSeq   = MobSub.MsSeq AND
            MServiceLimit.EndTS  >= Func.Common:mMakeTS(), 
      FIRST ServiceLimit NO-LOCK USE-INDEX SLSeq WHERE
            ServiceLimit.SLSeq = MServiceLimit.SLSeq AND
            ServiceLimit.GroupCode = lcTariffContract:
      Func.Common:mSplitTS(MServiceLimit.FromTS, OUTPUT odaActDate, OUTPUT liTime).
      RETURN ServiceLimit.GroupCode.
   END.
   
   RETURN "".
END.

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
                           Func.Common:mMakeTS()).
END FUNCTION.

FUNCTION fGetDataBundleInOrderAction RETURNS CHAR
   (iiOrderId    AS INT,
    icBundleType AS CHAR):

   DEF VAR lcContracts AS CHAR NO-UNDO.

   lcContracts = fGetBundles(icBundleType).
   IF lcContracts = "" THEN RETURN "".

   FIND FIRST OrderAction WHERE
              OrderAction.Brand = Syst.Var:gcBrand AND
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
    ideActStamp  AS DEC):

   DEF BUFFER bMsRequest FOR MsRequest.

   DEF VAR ldaReqDate    AS DATE NO-UNDO.
   DEF VAR liReqTime     AS INT  NO-UNDO.

   DEF VAR lcPostpaidDataBundles  AS CHAR NO-UNDO.
   DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.

   Func.Common:mSplitTS(ideActStamp,OUTPUT ldaReqDate,OUTPUT liReqTime).

   IF liReqTime > 0 THEN
      ideActStamp = Func.Common:mMake2DT(ldaReqDate + 1,0).

   ASSIGN lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
          lcDataBundleCLITypes  = fCParamC("DATA_BUNDLE_BASED_CLITYPES").

   /* Check STC Request with data bundle */
   FIND FIRST bMsRequest NO-LOCK WHERE
              bMsRequest.MsSeq   = iiMsSeq AND
              bMsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
              LOOKUP(STRING(bMsRequest.ReqStat),"4,9,99,3") = 0 AND
              bMsRequest.ActStamp = ideActStamp USE-INDEX MsSeq NO-ERROR.
   IF AVAIL bMsRequest AND
      (LOOKUP(bMsRequest.ReqCparam2,lcDataBundleCLITypes) > 0 OR
       LOOKUP(bMsRequest.ReqCparam5,lcPostpaidDataBundles) > 0) THEN
      RETURN TRUE.

   /* Check BTC Request with data bundle */
   FIND FIRST bMsRequest NO-LOCK WHERE
              bMsRequest.MsSeq   = iiMsSeq AND
              bMsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
              LOOKUP(STRING(bMsRequest.ReqStat),"4,9,99,3") = 0 AND
              bMsRequest.ActStamp = ideActStamp USE-INDEX MsSeq NO-ERROR.
   IF AVAIL bMsRequest AND
      LOOKUP(bMsRequest.ReqCparam2,lcPostpaidDataBundles) > 0 THEN
      RETURN TRUE.

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
                            PrePaidRequest.Brand     = Syst.Var:gcBrand AND
                            PrepaidRequest.PPRequest = liRequest)
      THEN LEAVE.
   END. /* DO WHILE TRUE: */
   
   ideAdjustBal = -1 * ideAdjustBal * 100.
 
   CREATE PrePaidRequest.
   ASSIGN
      PrePaidRequest.TSRequest   = Func.Common:mMakeTS()
      PrePaidRequest.UserCode    = Syst.Var:katun
      PrePaidRequest.Brand       = Syst.Var:gcBrand
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

   RUN Gwy/pp_platform.p(Syst.Var:gcBrand,PrePaidRequest.PPRequest).
   
   lcXML = RETURN-VALUE.
      
   IF lcXML > "" AND NOT lcXML BEGINS "ERR:" THEN
      liRespCode = INT(fGetRPCNodeValue(lcXML,"responseCode")) NO-ERROR.

   ASSIGN
      PrePaidRequest.Response   = lcXML
      PrePaidRequest.RespCode   = liRespCode
      PrePaidRequest.TSResponse = Func.Common:mMakeTS().

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
         
      RELEASE TopUpQueue.

      CREATE Memo.
      ASSIGN
         Memo.CreStamp  = Func.Common:mMakeTS()
         Memo.Brand     = Syst.Var:gcBrand
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(bMobSub.MsSeq)
         Memo.CustNum   = bMobSub.CustNum
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = Syst.Var:katun
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
      RUN Gwy/balancequery.p(icCLI).
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
   ASSIGN OrderAction.Brand = Syst.Var:gcBrand
          OrderAction.OrderId = iiOrderId
          OrderAction.ItemType = icType
          OrderAction.ItemKey = icKey
          OrderAction.ItemParam = icParam.
   RETURN TRUE.

END.

&ENDIF
