/* fctchange.i      16.12.04/aam

   checkings and values associated to clitype change 
   fctserval.i and cparam2.i needed

   changes:         04.01.05/aam only web-requests counted in FreePerYear
                    15.03.05/aam ignore requests scheduled before 3 am.    
                    02.08.05/aam allow changes to yesterday
                    06.02.06/aam change times are counted from beginning of 
                                 current year 
                    04.04.06/aam periodical contract may prohibit the change
*/
&IF "{&fctchange}" NE "YES"
&THEN
&GLOBAL-DEFINE fctchange YES
{Func/cparam2.i}
{Func/fctserval.i}
{Func/matrix.i}
{Syst/tmsconst.i}
{Mnp/mnp.i}
{Mm/requestaction_exec.i}
{Func/fcustpl.i}
{Func/penaltyfee.i}
{Func/stc_extension.i}
{Func/istc.i}
{Func/main_add_lines.i}
{Func/profunc.i}

/* ount number of requests */
FUNCTION fCountRequest RETURNS INTEGER
   (iiMsSeq     AS INT,
    icCLIType   AS CHAR,
    icAttribute AS CHAR,
    idFromDate  AS DEC,
    icUser      AS CHAR).

   DEF VAR liReqQty   AS INT  NO-UNDO. 
   DEF VAR llAllowChg AS LOG  NO-UNDO. 
   DEF VAR lcReqQty   AS CHAR NO-UNDO. 

   DEF BUFFER bCTRequest FOR MsRequest.
   
   lcReqQty = fServAttrValue(icCLIType,
                             "TypeChg",
                             icAttribute,
                             OUTPUT llAllowChg).

   IF lcReqQty NE ? 
   THEN liReqQty = INTEGER(lcReqQty) NO-ERROR.
   ELSE liReqQty = 0.
      
   /* are there changes for this subscription left */
   IF liReqQty > 0 THEN DO:
         
      /* possible current request will be counted in this loop also */
      FOR EACH bCTRequest NO-LOCK WHERE
               bCTRequest.MsSeq      = iiMsSeq        AND 
               bCTRequest.ReqType    = 0              AND
               /* disregard errorneus and cancelled */
               bCTRequest.ReqStatus < 3               AND
               bCTRequest.ActStamp  >= idFromDate:

         IF icUser > "" AND NOT bCTRequest.UserCode BEGINS icUser THEN NEXT.
         
         liReqQty = liReqQty - 1. 
      END.
   END. 
 
   RETURN MAX(liReqQty,0).
   
END FUNCTION.

/* how many free changes per year */
FUNCTION fFreeChanges RETURNS INTEGER
   (iiMsSeq   AS INT,
    icCLIType AS CHAR,
    idtActDate   AS DATE).

   DEF VAR ldFromDate AS DEC NO-UNDO. 
 
   /* counted from beginning of current year */
   idtActDate = DATE(1,1,YEAR(idtActDate)).
                     
   ldFromDate = YEAR(idtActDate) * 10000 + 
                MONTH(idtActDate) * 100  +
                DAY(idtActDate) + 
                /* scheduled requests in the beginning of 1. day are ignored */
                0.10801. 
      
   RETURN fCountRequest(iiMsSeq,
                        icCLIType,
                        "FreePerYear",
                        ldFromDate,
                        "WEB").   /* only requests made in web are counted */
                        
END.
                        
FUNCTION fChangePerMonth RETURNS INTEGER
   (iiMsSeq   AS INT,
    icCLIType AS CHAR,
    idtActDate   AS DATE).

   DEF VAR ldFromDate AS DEC  NO-UNDO. 
   
   FOR EACH DayCampaign WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeModel NE "" AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK:
      IF CAN-FIND(FIRST DCCLI WHERE
                        DCCLI.Brand = Syst.Var:gcBrand AND
                        DCCLI.DCEvent = DayCampaign.DCEvent AND
                        DCCLI.MsSeq = iiMsSeq AND
                        DCCLI.ValidFrom <= idtActDate AND
                        DCCLI.ValidTo >= idtActDate)
      THEN RETURN 0.
   END.

   /* changes within one calendar month */ 
   ASSIGN idtActDate = DATE(MONTH(idtActDate),1,YEAR(idtActDate))
          ldFromDate = YEAR(idtActDate) * 10000 + 
                       MONTH(idtActDate) * 100  +
                       DAY(idtActDate) + 
                       /* scheduled requests in the beginning of 1. day 
                          are ignored */
                       0.10801. 

       
   RETURN fCountRequest(iiMsSeq,
                        icCLIType,
                        "PerMonth",
                        ldFromDate,
                        "").
                        
END.

FUNCTION fIsSTCAllowed RETURNS LOGIC
   (iiMsSeq       AS INT,
    icOldCLIType  AS CHAR,
    icNewCLIType  AS CHAR,
    OUTPUT ocInfo AS CHAR):

   DEF VAR lcResult         AS CHAR NO-UNDO.
   DEF VAR lcIPLContracts   AS CHAR NO-UNDO.

   DEF BUFFER bBTC FOR MsRequest.

   lcIPLContracts = fCParamC("IPL_CONTRACTS").

   /* 1. STC (Any Voice to Other) request is not allowed if Upgrade BONO BTC */
   /* 2. STC (IPL to Other) request is not allowed if Upgrade IPL BTC */
   FOR FIRST bBTC NO-LOCK WHERE
             bBTC.MsSeq   = iiMsSeq AND
             bBTC.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
             LOOKUP(STRING(bBTC.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0 AND
             bBTC.ReqCparam5 > "":
      IF LOOKUP(bBTC.ReqCparam1,lcIPLContracts) > 0 OR
         fMatrixAnalyse(Syst.Var:gcBrand,
                        "BTC-DENY",
                        "SubsTypeFrom;SubsTypeTo",
                        icOldCLIType + ";" + icNewCLIType,
                        OUTPUT lcResult) = 0 THEN DO:
         ocInfo = "Subscription has a pending BTC with upgrade upsell. " +
                  "Subscription Type change is not allowed".
         RETURN FALSE.
      END.
   END.

   RETURN TRUE.

END FUNCTION. /* FUNCTION fIsSTCAllowed RETURNS LOGIC */
   
FUNCTION fValidateMobTypeCh RETURNS LOGICAL
   (INPUT  iiMsSeq           AS INT,
    INPUT  icNewCLIType      AS CHAR,
    INPUT  ideSTCtamp        AS DEC,
    INPUT  ilExtendContract  AS LOG,
    INPUT  plByPassTypeCheck AS LOG,
    INPUT  piOrderID         AS INT,
    INPUT  icReqSource       AS CHAR,
    INPUT  llgMerge          AS LOG, 
    OUTPUT ocError           AS CHAR):

   DEF VAR ocResult AS CHARACTER NO-UNDO.
   DEF VAR lcMNP AS CHARACTER NO-UNDO. 
   DEF VAR ldaDueDate AS DATE NO-UNDO. 
   DEF VAR liCount AS INT NO-UNDO. 
   DEF VAR liMonths AS INT NO-UNDO. 
   DEF VAR ldaSTCDate AS DATE NO-UNDO. 
   DEF VAR liTime AS INT NO-UNDO. 

   DEFINE BUFFER NewCLIType  FOR CLIType.
   DEF BUFFER MNPProcess FOR MNPProcess.

   IF ideSTCtamp EQ ? THEN ideSTCtamp = Func.Common:mMakeTS().

   Func.Common:mSplitTS(ideSTCtamp, OUTPUT ldaSTCDate, OUTPUT liTime). 

   FIND FIRST mobsub NO-LOCK WHERE
              mobsub.msseq = iiMsSeq NO-ERROR.
   IF NOT AVAIL mobsub THEN DO:
      ocError = "Subscription not found".
      RETURN FALSE.
   eND.
   FIND FIRST Customer WHERE
              Customer.custnum EQ Mobsub.custnum NO-LOCK NO-ERROR.           
   /* 1 */
   FIND FIRST NewCliType WHERE
              NewCLIType.Brand = Syst.Var:gcBrand AND
              NewCLIType.CLIType = icNewCLIType NO-LOCK NO-ERROR.
   IF NOT AVAIL NewCLIType THEN DO:
      ocError = "Unknown or missing clitype!".
      RETURN FALSE.
   END.    

   IF NOT plByPassTypeCheck AND
      LOOKUP(STRING(NewCLIType.StatusCode),
            {&CLITYPE_STC_ACTIVE_STATUSES}) = 0
   THEN DO:
      ocError = "Function not allowed due to business rules!".
      RETURN FALSE.
   END.
 
   IF AVAIL Customer AND fIsPro(Customer.category) THEN DO:
      IF fValidateProSTC(MobSub.Custnum,
                         MobSub.CliType,
                         NewCLIType.Clitype) > "" THEN DO:
         ocError = "Function not allowed due to business rules!".
         RETURN FALSE.
      END. 
   END.
   ELSE DO:
      IF NOT llgMerge                                  AND 
         fIsConvergenceTariff(NewCLIType.Clitype)      AND 
         fIsConvergenceTariff(MobSub.CLItype) EQ FALSE AND
         piOrderID                            EQ 0     THEN DO:
         ocError = "Function not allowed due to business rules!".
         RETURN FALSE.
      END.
   END.

   /* partial convergent to mobile */
   IF NOT llgMerge                                       AND
      (mobsub.msstatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR
       mobsub.msstatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) AND 
       piOrderID       EQ 0                              THEN DO:

      IF CAN-FIND(FIRST Order NO-LOCK WHERE
                        Order.MsSeq = mobsub.msseq AND
                        Order.CLIType = mobsub.CliType AND
                        Order.OrderType NE {&ORDER_TYPE_RENEWAL} AND
                 LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) = 0)
      THEN
         ocError = "STC is not allowed due to convergent order is still ongoing".
      ELSE 
         ocError = "STC is not allowed since subscription is Fixed only".

      RETURN FALSE.
   END.      
  
   /* 2 */
   IF NOT fIsSTCAllowed(INPUT Mobsub.MsSeq,
                        INPUT Mobsub.CLIType,
                        INPUT icNewCLIType,
                        OUTPUT ocError) THEN RETURN FALSE.

   /* 3 */
   RUN Mm/requestaction_check.p(0,MobSub.CLIType,Mobsub.MsSeq,
                             icReqSource, OUTPUT ocError).
   IF ocError NE "" THEN RETURN FALSE.
   
   /* 4 */
   lcMNP = Mnp.MNPOutGoing:mGetMNPOutOngoing(MobSub.CLI).
   IF lcMNP NE "" THEN DO: 
      
      FIND MNPProcess WHERE
           MNPProcess.PortRequest = lcMNP NO-LOCK NO-ERROR.

      IF AVAIL MNPProcess THEN DO:

         IF 1 > fMNPPeriods(
            input Func.Common:mMakeTS(),
            input MNPProcess.PortingTime,
            INPUT 0,
            OUTPUT ldaDueDate) THEN DO: 
            ocError = "Ongoing MNP OUT request. Too late for STC".
            RETURN FALSE.
         END.

      END.

   END.

   /* YBU-3187 */
   IF CAN-FIND(FIRST Order NO-LOCK WHERE
                     Order.MsSeq = iiMsSeq AND
                     Order.OrderType = {&ORDER_TYPE_STC} AND
                     Order.OrderId NE piOrderID AND
              LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0) THEN DO:
      ocError = "Ongoing STC order".
      RETURN FALSE.
   END.

   IF ilExtendContract AND NOT  
      fCanExtendTerminalContract(
          BUFFER MobSub,
          ldaSTCDate,
          icNewCLIType,
          OUTPUT ocError) THEN RETURN FALSE.

   IF DAY(ldaSTCDate) <> 1 AND NOT fIsiSTCAllowed(INPUT Mobsub.MsSeq) THEN 
   DO:
       ocError = "Multiple immediate STC is not allowed in same month due to business rules!".
       RETURN FALSE.
   END.

   RETURN TRUE.

END FUNCTION. 


FUNCTION fChkSTCPerContr RETURNS LOGICAL:
   
   DEFINE VARIABLE lcTiePeriod AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE ok AS LOGICAL NO-UNDO.

   /* is there a time limit for scheduling (on old type) */
   lcTiePeriod = fServAttrValue(MobSub.CLIType,
                                  "Typechg",
                                  "TiePeriod",
                                   OUTPUT ok).
          
   IF lcTiePeriod = "1" THEN
      FOR EACH DayCampaign WHERE
               DayCampaign.Brand = Syst.Var:gcBrand AND
               DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
               DayCampaign.TermFeeModel NE "" AND
               DayCampaign.TermFeeCalc > 0 NO-LOCK:
         FIND FIRST DCCLI WHERE
                    DCCLI.Brand = Syst.Var:gcBrand AND
                    DCCLI.DCEvent = DayCampaign.DCEvent AND
                    DCCLI.MsSeq = MobSub.MsSeq AND
                    DCCLI.ValidFrom <= TODAY AND
                    DCCLI.ValidTo >= TODAY NO-LOCK NO-ERROR.
         IF AVAIL DCCLI THEN RETURN TRUE.
      END.

   RETURN FALSE.

END FUNCTION. 

FUNCTION fValidateNewCliType RETURNS INT
(INPUT icNewCliType AS CHAR,
 INPUT icDataBundleId AS CHAR,
 INPUT ilByPass     AS LOG,
 OUTPUT ocError     AS CHAR):

   DEFINE VARIABLE ocResult        AS CHAR NO-UNDO.
   DEF VAR llPrimaryActive         AS LOG  NO-UNDO.

   DEF BUFFER lbMobSub             FOR MobSub.
   DEF BUFFER bCTAgrCust FOR Customer.
   DEF BUFFER DayCampaign FOR DayCampaign.
   DEF BUFFER CLIType FOR CLiType.
   DEF BUFFER bTariffType FOR CLiType.
   DEF VAR liLineType AS INT NO-UNDO. 
   DEF VAR ldaCont15PromoEnd AS DATE NO-UNDO. 

   /* 1 */
   FIND FIRST CLIType WHERE 
              CliType.Brand   = Syst.Var:gcBrand AND 
              CLIType.Clitype = icNewCliType 
   NO-LOCK NO-ERROR.
   IF NOT AVAIL CLIType THEN DO:
      ocError = "Unknown or missing clitype!".
      RETURN 2.
   END.
   
   IF icDataBundleId > "" THEN DO:

      FIND FIRST bTariffType WHERE
                 bTariffType.Brand   = Syst.Var:gcBrand AND 
                 bTariffType.Clitype = icDataBundleId 
      NO-LOCK NO-ERROR.
      IF NOT AVAIL bTariffType THEN DO:
         ocError = "Unknown or missing clitype!".
         RETURN 2.
      END.
      liLineType = bTariffType.LineType.
   END.
   ELSE liLineType = CLIType.LineType.
   
   /* 2 */
   IF Mobsub.CliType = icNewCliType THEN DO:
      ocError = "New clitype cannot be same as the old one!".
      RETURN 1.
   END.

   /* 3 */
   FIND FIRST bCTAgrCust WHERE 
              bCTAgrCust.CustNum = Mobsub.AgrCust NO-LOCK NO-ERROR.
   IF fMatrixAnalyse( INPUT Syst.Var:gcBrand,
                        "STC-IDType",
                        "ID;SubsTypeFrom;SubsTypeTo",
                        bCTAgrCust.CustIDType + ";" +
                        Mobsub.CliType      + ";" + 
                        icNewCliType           ,
                     OUTPUT ocResult) = 0 
   THEN DO:
      ocError = {&MSG_NOT_ALLOWED}.
      RETURN 3.
   END.

   /* 4 - STC not allowed to additional line if no main line active */
   IF liLineType EQ {&CLITYPE_LINETYPE_ADDITIONAL} THEN DO:

      /* If main line is not active, don't allow STC */
      MOBSUB_LOOP:
      FOR EACH lbMobSub NO-LOCK WHERE
               lbMobSub.Brand   = Syst.Var:gcBrand AND
               lbMobSub.InvCust = Mobsub.CustNum AND
               lbMobSub.PayType = FALSE AND
               lbMobSub.MsSeq NE Mobsub.MsSeq,
         FIRST bTariffType NO-LOCK WHERE
               bTariffType.Brand = Syst.Var:gcBrand AND
               bTariffType.CLIType = lbMobSub.TariffBundle AND
               bTariffType.LineType = {&CLITYPE_LINETYPE_MAIN}:

         IF fHasPendingRequests(
            lbMobSub.MsSeq,
            lbMobSub.CLI,
            bTariffType.LineType) THEN NEXT.

         llPrimaryActive = TRUE.
         LEAVE.
      END.

      IF NOT llPrimaryActive THEN DO:
         ocError = "STC is not allowed to additional line because main line is not active".
         RETURN 4.
      END.
   END.

   /* 5 - Validate ByPass Flag */
   IF NOT ilByPass THEN DO:
      ldaCont15PromoEnd  = fCParamDa("CONT15PromoEndDate").
      IF CLIType.WebStatusCode NE {&CLITYPE_WEBSTATUSCODE_ACTIVE}
         AND NOT (CLITYPE.CLITYPE EQ "CONT15" AND
                  CLIType.WebStatusCode EQ 2 AND 
                  ldaCont15PromoEnd NE ? AND
                  TODAY <= ldaCont15PromoEnd) THEN DO:
         ocError = {&MSG_NOT_ALLOWED}.
         RETURN 5.
      END.
      ELSE IF icDataBundleId > "" THEN DO:

         FIND DayCampaign NO-LOCK WHERE
              DayCampaign.Brand = Syst.Var:gcBrand AND
              DayCampaign.DcEvent = icDataBundleId NO-ERROR.
         IF NOT AVAIL DayCampaign OR
                      DayCampaign.StatusCode NE {&DAYCAMPAIGN_STATUSCODE_ACTIVE} THEN DO:
            ocError = {&MSG_NOT_ALLOWED}.
            RETURN 6.
         END.
      END.
   END.
   
   ocError = "".
   RETURN 0.
END.

FUNCTION fGetCLITypeAtTermDiscount RETURNS CHAR
    (BUFFER pbDCCLI FOR DCCLI):

   DEFINE VARIABLE lcBundle AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE lcCLIType AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE ldaActivatedDate AS DATE NO-UNDO.
   DEFINE VARIABLE lcBundleCLITypes AS CHARACTER NO-UNDO.

   DEF BUFFER MsOwner FOR MsOwner.
   
   ASSIGN ldTS = Func.Common:mHMS2TS(pbDCCLI.ValidFrom,"23:59:59")
          lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").
   
   FIND FIRST MsOwner WHERE
              MSOwner.MsSeq = pbDCCLI.MsSeq AND 
              MsOwner.TSBegin <= ldTS NO-LOCK NO-ERROR. 
   IF AVAIL MsOwner THEN DO:
      lcCLIType = MSOwner.CLIType .
   END.
   ELSE DO:
      FIND LAST MsOwner WHERE
                MSOwner.MsSeq = pbDCCLI.MsSeq AND 
                MsOwner.TSBegin > ldTS NO-LOCK NO-ERROR. 
      IF AVAIL MsOwner THEN DO:
         lcCLIType = MsOwner.CLIType.
      END.
   END.
   
   IF lcCLIType EQ "CONTRD1" THEN lcCLIType = "CONTRDCONTDATA".
   ELSE IF lcCLIType EQ "CONTRD2" THEN lcCLIType = "CONTRDCONTD2".
   ELSE IF lcCLIType EQ "CONTRD3" THEN lcCLIType = "CONTRDCONTD3".
   /* Fix for YBU-1239 */
   ELSE IF LOOKUP(lcCLIType,lcBundleCLITypes) > 0 THEN DO:
      lcBundle = fGetOriginalBundle(pbDCCLI.MsSeq, 
                                    pbDCCLI.DcEvent,
                                    TODAY,
                                    OUTPUT ldaActivatedDate).
      IF NUM-ENTRIES(lcBundle) > 1 THEN lcBundle = "".
      CASE lcBundle:
         WHEN "CONTDATAACT" THEN lcBundle = "CONTDATA".
         WHEN "CONTD2ACT" THEN lcBundle = "CONTD2".
      END.     
      lcCLIType = lcCLIType + lcBundle.
   END. /* IF LOOKUP(lcCLIType,lcBundleCLITypes) > 0 THEN DO: */

   RETURN lcCLIType. 

END FUNCTION.
                  
FUNCTION fCheckSTCBundle RETURNS LOG
         (INPUT icCLIType AS CHAR,
          INPUT icBundle AS CHAR,
          OUTPUT ocError AS CHAR) :

  DEF VAR lcIPLContracts   AS CHAR NO-UNDO.
  DEF VAR lcCONTDContracts AS CHAR NO-UNDO.
  DEF VAR lcFLATContracts  AS CHAR NO-UNDO.
  DEF VAR lcCONTSContracts AS CHAR NO-UNDO.
  DEF VAR lcCONTSFContracts AS CHAR NO-UNDO.
  DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
  
  ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
         lcCONTDContracts = fCParamC("CONTD_CONTRACTS")
         lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
         lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
         lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS")
         lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").
                  
  IF (icCLIType EQ "CONTF" AND LOOKUP(icBundle,lcFLATContracts) = 0) OR
     (icCLIType EQ "CONTD" AND LOOKUP(icBundle,lcCONTDContracts) = 0) OR
     (icCLIType EQ "CONTRD" AND LOOKUP(icBundle,lcIPLContracts) = 0) OR
     (icCLIType EQ "CONTS" AND LOOKUP(icBundle,lcCONTSContracts) = 0) OR
     (icCLIType EQ "CONTSF" AND LOOKUP(icBundle,lcCONTSFContracts) = 0) OR
     LOOKUP(icCLIType,lcBundleCLITypes) = 0
  THEN DO:
     ocError = "Bundle is not allowed for this subscription type " + icBundle.
     RETURN FALSE.
  END.

  RETURN TRUE.

END.

&ENDIF
