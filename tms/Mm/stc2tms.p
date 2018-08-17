/* stc2tms.p SUBSCRIPTION TYPE CHANGE STC Request handling
   changes:
      22.sep.2015 hugo.lujan - YPR-2521 - [Q25] - TMS - Subscription
       termination/ MNP out porting, STC (postpaid to prepaid)
*/
&SCOPED-DEFINE STC TRUE

{Syst/commali.i}
{Func/service.i}
{Mm/fbundle.i}
{Func/ftmrlimit.i}
{Mc/invoicetarget.i}
{Rate/rerate_request.i}
{Func/customerextralinefunc.i}
{Func/fcpfat.i}
{Func/servcomfee.i}
{Func/addline_discount.i}
{Func/fbankdata.i}
{Func/barrfunc.i}
{Func/profunc_request.i}
{Func/add_lines_request.i}
{Func/custfunc.i}
{Func/order.i}
/* 21.2.2018 Migration is not used, commented the include file
{Migration/migrationfunc.i}*/

DEFINE INPUT PARAMETER iiMSRequest AS INTEGER NO-UNDO.

{Func/remfees.i}

DEF BUFFER bMergeMobSub   FOR MobSub.
DEF BUFFER bMergeDCCLI    FOR DCCLI.
DEF BUFFER bMergedMsOwner FOR MsOwner.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobsub AS HANDLE NO-UNDO.
   lhMobsub = BUFFER Mobsub:HANDLE.

   DEFINE VARIABLE lhMsOwner AS HANDLE NO-UNDO.
   lhMsOwner = BUFFER MsOwner:HANDLE.

   DEFINE VARIABLE lhSubSer AS HANDLE NO-UNDO.
   lhSubSer = BUFFER SubSer:HANDLE.

   DEFINE VARIABLE lhSubSerPara AS HANDLE NO-UNDO.
   lhSubSerPara = BUFFER SubSerPara:HANDLE.

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   
   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.

   DEFINE VARIABLE lhMergeMobSub AS HANDLE NO-UNDO.
   lhMergeMobSub = BUFFER bMergeMobSub:HANDLE.

   DEFINE VARIABLE lhMergeDCCLI AS HANDLE NO-UNDO.
   lhMergeDCCLI = BUFFER bMergeDCCLI:HANDLE.

   DEFINE VARIABLE lhMergedMsOwner AS HANDLE NO-UNDO.
   lhMergedMsOwner = BUFFER bMergedMsOwner:HANDLE.

END.

DEF VAR lcAttrValue        AS CHAR NO-UNDO. 
DEF VAR lcError            AS CHAR NO-UNDO.
DEF VAR ldEndStamp         AS DEC  NO-UNDO.
DEF VAR ldBegStamp         AS DEC  NO-UNDO.
DEF VAR llOldPayType       AS LOG  NO-UNDO.
DEF VAR liOrigStatus       AS INT  NO-UNDO.
DEF VAR ldaNewBeginDate    AS DATE NO-UNDO.
DEF VAR ldeActStamp        AS DEC  NO-UNDO.

DEF VAR ldaNextMonthActDate AS DATE NO-UNDO.
DEF VAR ldNextMonthActStamp AS DEC  NO-UNDO.
DEF VAR liOrderId           AS INT  NO-UNDO.
DEF VAR lcResult            AS CHAR NO-UNDO.

DEF BUFFER bOldType  FOR CLIType.
DEF BUFFER bNewTariff FOR CLIType.
DEF BUFFER bOldTariff FOR CLIType.
DEF BUFFER bCLIType FOR CLIType.

DEF TEMP-TABLE ttContract NO-UNDO
    FIELD DCEvent AS CHAR.

/********** Main start *********/

FIND FIRST MSRequest WHERE
           MSRequest.MSrequest = iiMSrequest NO-LOCK NO-ERROR.

IF MsRequest.ReqType NE {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN 
   RETURN "ERROR".

liOrigStatus = MsRequest.ReqStatus.
IF LOOKUP(STRING(liOrigStatus),"6,8") = 0 THEN RETURN "ERROR".

IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

ldeActStamp = MsRequest.ActStamp.

Func.Common:mSplitTS(MsRequest.ActStamp,
         OUTPUT ldtActDate,
         OUTPUT liActTime).

IF liActTime > 0 THEN 
   ldEndStamp = Func.Common:mMake2DT(ldtActDate,liActTime - 1).
ELSE
   ldEndStamp = Func.Common:mMake2DT(ldtActDate - 1,86399).

IF liOrigStatus = 8 AND MsRequest.ReqIParam2 > 0 THEN DO:
   FIND FIRST Order NO-LOCK WHERE
              Order.Brand = Syst.Var:gcBrand AND
              Order.OrderID = MsRequest.ReqIparam2 NO-ERROR.
   IF NOT AVAIL Order THEN DO:
      fReqError(SUBST("Order not found: &1", MsRequest.ReqIParam2)).
      RETURN.
   END.
/* 21.2.2018 Migration is not used, commented the code
   and also the include file
   ELSE IF fIsNumberInMigration(Order.CLI) EQ TRUE THEN DO:
      fReqError(SUBST("Order is in migration phase: &1", MsRequest.ReqIParam2)).
      RETURN.
   END.
*/
   ELSE IF LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 THEN DO:
      IF Order.StatusCode EQ {&ORDER_STATUS_MNP_RETENTION}     OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_MAIN_LINE} THEN DO: /* ADDLINE-19 Additional Line Renewal case handling */
         ASSIGN ldaNextMonthActDate  = (Func.Common:mLastDayOfMonth(ldtActDate) + 1)
                ldNextMonthActStamp  = Func.Common:mMake2DT(ldaNextMonthActDate,0).
         FIND CURRENT MsRequest EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL MsRequest THEN DO:
            ASSIGN MsRequest.ActStamp   = ldNextMonthActStamp
                   MsRequest.ReqDParam1 = ldNextMonthActStamp.
            fReqStatus(8,"").
         END.
         RETURN.
      END. /* IF Order.StatusCode EQ {&ORDER_STATUS_MNP_RETENTION} THEN DO:*/
      ELSE IF Order.StatusCode EQ {&ORDER_STATUS_RENEWAL_STC_COMPANY} THEN DO:
         fReqError("Pending renewal order").
         RETURN.
      END.
   END.
   ELSE IF Order.OrderType EQ {&ORDER_TYPE_STC} AND
           Order.StatusCode NE {&ORDER_STATUS_ONGOING} THEN DO:
      fReqError("Order is in wrong status: " + Order.StatusCode).
      RETURN.
   END. /* IF AVAIL Order THEN DO: */
END.
   
IF liOrigStatus = 8 AND fChkSubRequest(iiMSRequest) = FALSE THEN DO:
   fReqStatus(7,"").
   RETURN.
END.

FIND Mobsub WHERE Mobsub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.

IF NOT AVAILABLE Mobsub THEN DO:
   fReqError("Mobsub not found").
   RETURN.
END.

/* nothing to do */
IF MsRequest.ReqCParam1 = MsRequest.ReqCParam2 OR
   MsRequest.ReqCParam1 = ""                   OR
   MsRequest.ReqCParam2 = "" 
THEN DO:
   fReqError("CLITypes are not valid").
   RETURN.
END.
             
/* is timing correct; if either type should be changed only on 
   1. of month then accept only that */
lcAttrValue = fChkTiming(MsRequest.ReqCParam1,
                         MsRequest.ReqCParam2,
                         ldtActDate). 
IF lcAttrValue > "" THEN DO:
   fReqError(lcAttrValue).
   RETURN.
END.
      
FIND bOldType WHERE
     bOldType.Brand   = Syst.Var:gcBrand AND
     bOldType.CLIType = MsRequest.ReqCParam1 NO-LOCK NO-ERROR.

IF NOT AVAILABLE bOldType THEN DO:
   fReqError("Unknown CLIType " + MsRequest.ReqCParam1).
   RETURN.
END. 

FIND CLIType WHERE
     CLIType.Brand   = Syst.Var:gcBrand AND
     CLIType.CLIType = MsRequest.ReqCParam2 NO-LOCK NO-ERROR.

IF NOT AVAILABLE CLIType THEN DO:
   fReqError("Unknown CLIType " + MsRequest.ReqCParam2).
   RETURN.
END. 

IF MsRequest.ReqCParam5 > "" THEN
   FIND bNewTariff NO-LOCK WHERE
        bNewTariff.Brand   = Syst.Var:gcBrand AND
        bNewTariff.CLIType = MsRequest.ReqCParam5 NO-ERROR.
ELSE
   FIND bNewTariff NO-LOCK WHERE
        ROWID(bNewTariff) = ROWID(CLIType) NO-ERROR.

IF NOT AVAILABLE bNewTariff THEN DO:
   fReqError("Unknown Tariff bundle " + MsRequest.ReqCParam5).
   RETURN.
END. 

llOldPayType = (bOldType.PayType = 2).
  
FIND Customer OF Mobsub NO-LOCK. 

/* first round of status 7/8 */
IF MsRequest.ReqCParam4 = "" THEN DO:

   IF liOrigStatus = 8 THEN DO:
      
      IF fIsConvergenceTariff(MobSub.CLIType) AND
         CLIType.TariffType EQ {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN DO:   

         liOrderId = fFindFixedLineOrder(MSRequest.MSSeq).         
         IF liOrderId EQ 0
            THEN lcResult = "OrderID not found".
         /* This call makes synchronous termination request to MuleDB */
         ELSE lcResult = fSendFixedLineTermReqToMuleDB(liOrderId).           

         IF lcResult NE "" THEN DO:  
            /* "Fixed number termination failed" */
            Func.Common:mWriteMemo("MobSub",
                                   STRING(MSrequest.MsSeq),
                                   MobSub.CustNum,
                                   "La baja del sevicio fijo ha fallado: ",
                                   lcResult).
            fReqError(SUBST("La baja del sevicio fijo ha fallado: &1", lcResult)).
            RETURN.
         END.
      END.

      RUN pNetworkAction.
      IF RETURN-VALUE BEGINS "SubRequest" THEN RETURN.
   END.

   IF liOrigStatus = 6 AND CLIType.CliType EQ "TARJ5" THEN DO:
      
      RUN pActivateTARJ5PromotionalPrice(OUTPUT lcerror).
      
      IF lcerror BEGINS "ERROR" THEN DO:
         fReqError("Promotional price activation failed: " + lcError).
         RETURN.
      END.
   END.
   
   /* NOTE: check before pUpdateSubscription */ 
   IF Mobsub.TariffBundle > "" THEN
      FIND bOldTariff NO-LOCK WHERE
           bOldTariff.Brand = Syst.Var:gcBrand AND
           bOldTariff.CLIType = Mobsub.TariffBundle NO-ERROR.
   ELSE 
      FIND bOldTariff NO-LOCK WHERE
           ROWID(bOldTariff) = ROWID(bOldType) NO-ERROR.
   IF NOT AVAIL bOldTariff THEN DO:
      fReqError("Unknown old CLIType").
      RETURN.
   END.
   
   IF MobSub.TerritoryOwner <> "FIBMM02" THEN
   DO:
       IF bOldType.FixedLineType EQ {&FIXED_LINE_TYPE_FIBER} AND 
          CLIType.FixedLineType  EQ {&FIXED_LINE_TYPE_FIBER} AND
          (bOldType.FixedLineDownload <> CLIType.FixedLineDownload OR bOldType.FixedLineUpload <> CLIType.FixedLineUpload) THEN
       DO:   
           RUN pSpeedChangeRequestForProvisioning.
           IF RETURN-VALUE NE "" THEN
           DO:
               fReqError(RETURN-VALUE).
               RETURN.
           END. 
       END.    
   END.

   RUN pInitialize.
   RUN pFeesAndServices.
   RUN pUpdateSubscription.

   IF MobSub.MultiSIMID    > 0                AND 
      NOT fCLITypeIsMainLine(CLIType.CLIType) AND 
      NOT fCLITypeIsExtraLine(CLIType.CLIType) THEN 
      RUN pMultiSimSTC (INPUT ldtActDate).
   ELSE IF bOldTariff.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
           bNewTariff.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL} THEN
      fAdditionalLineSTC(MsRequest.Msrequest,
                        Func.Common:mMake2DT(ldtActDate + 1,0),
                        "STC_FINAL").

   /* Remove additional line termination request when correct STC done */
   IF bNewTariff.LineType NE {&CLITYPE_LINETYPE_ADDITIONAL} THEN
      fRemoveAdditionalLineTerminationReq(MobSub.MsSeq).

   /* close periodical contracts that are not allowed on new type */
   RUN pCloseContracts(MsRequest.MsRequest,
                       MsRequest.ReqCParam2,
                       MsRequest.ReqCParam5,
                       MsRequest.MsSeq,
                       MobSub.CustNum,
                       ldtActDate,
                       ldEndStamp,
                       {&REQUEST_SOURCE_STC}).

   /* subrequests created -> second round of status 7/8 */
  IF RETURN-VALUE BEGINS "SubRequest" THEN DO:
      FIND Current MsRequest EXCLUSIVE-LOCK.
      MsRequest.ReqCParam4 = "SR".
      fReqStatus(7,"").
      RETURN.
   END. 
   
END.

RUN pFinalize.    

FINALLY:
   fCleanEventObjects().
END.

/********** Main end *********/


PROCEDURE pInitialize:

   IF llDoEvent THEN DO:
      RUN StarEventInitialize(lhMobsub).
      RUN StarEventInitialize(lhMsOwner).
      RUN StarEventInitialize(lhSubSer).
      RUN StarEventInitialize(lhSubSerPara).
      RUN StarEventInitialize(lhSingleFee).
      RUN StarEventInitialize(lhCustomer).
   END.
   
END PROCEDURE.

PROCEDURE pSpeedChangeRequestForProvisioning:
    DEF VAR liOrderId          AS INTE NO-UNDO.

    FOR EACH Order WHERE Order.MsSeq = Mobsub.MSSeq NO-LOCK 
        BY Order.CrStamp DESC:

        IF Order.StatusCode <> {&ORDER_STATUS_DELIVERED} THEN 
            NEXT.

        FIND FIRST FusionMessage NO-LOCK WHERE 
                   FusionMessage.OrderId        = Order.OrderId                      AND 
                   FusionMessage.MessageType    = {&FUSIONMESSAGE_TYPE_CREATE_ORDER} AND 
                   FusionMessage.MessageStatus  = {&FUSIONMESSAGE_STATUS_HANDLED}    NO-ERROR.
        IF NOT AVAIL FusionMessage THEN 
            NEXT.
                       
        ASSIGN liOrderId = Order.OrderId.

        LEAVE.
    END.

    IF liOrderId > 0 THEN
    DO:
        RUN Gwy/masmovil_speed_change.p(Order.OrderId, CLIType.FixedLineDownload, CLIType.FixedLineUpload).
        IF RETURN-VALUE NE "" THEN
            RETURN "Fixed line fiber speed change request failed with " + RETURN-VALUE.
        ELSE 
            RETURN "".    
    END.

    RETURN "Fixed line order failed to identify.".    

END PROCEDURE.

PROCEDURE pFeesAndServices:

   DEF VAR ldtFeeDate         AS DATE NO-UNDO. 
   DEF VAR lcOldBroken        AS CHAR NO-UNDO.
   DEF VAR lcOldChgMonth      AS CHAR NO-UNDO.
   DEF VAR lcOldTimeLim       AS CHAR NO-UNDO. 
   DEF VAR lcOldBillItem      AS CHAR NO-UNDO. 
   DEF VAR lcNewBroken        AS CHAR NO-UNDO.
   DEF VAR lcNewChgMonth      AS CHAR NO-UNDO. 
   DEF VAR liCredQty          AS INT  NO-UNDO. 
   DEF VAR liTimeLimit        AS INT  NO-UNDO. 
   DEF VAR ldaPrevMonth       AS DATE NO-UNDO. 
   DEF VAR liPrevPeriod       AS INT  NO-UNDO. 
   DEF VAR liRequest          AS INT  NO-UNDO.
   DEF VAR lcResult           AS CHAR NO-UNDO.
   DEF VAR llAddLineDisc      AS LOG  NO-UNDO.
   DEF VAR lcAddLineDisc      AS CHAR NO-UNDO.
   DEF BUFFER bMember FOR DPMember.
  
   /* first handle services that are not on subscription level;
      fees etc. */

   ASSIGN 
   lcOldBroken   = fServAttrValue(MsRequest.ReqCParam1,
                                  "MFEE",
                                  "BrokenMonth",
                                  OUTPUT llAllowed) 
   /* should whole change month be billed with these fees */
   lcOldChgMonth = fServAttrValue(MsRequest.ReqCParam1,
                                  "TYPECHG",
                                  "ChgMonthFee",
                                  OUTPUT llAllowed)
   lcOldTimeLim  = fServAttrValue(MsRequest.ReqCParam1,
                                  "TYPECHG",
                                  "SchedTime",
                                  OUTPUT llAllowed)

   /* same values for new clitype */
   lcNewBroken   = fServAttrValue(MsRequest.ReqCParam2,
                                  "MFEE",
                                  "BrokenMonth",
                                  OUTPUT llAllowed) 
   lcNewChgMonth = fServAttrValue(MsRequest.ReqCParam2,
                                  "TYPECHG",
                                  "ChgMonthFee",
                                  OUTPUT llAllowed)
   lcOldBillItem = bOldType.FeeModel1.

   IF lcOldTimeLim > "" 
   THEN liTimeLimit = INTEGER(ENTRY(1,lcOldTimeLim,".")) * 3600 +
                      INTEGER(ENTRY(2,lcOldTimeLim,".")) * 60. 
   ELSE liTimeLimit = 0.

   /* change month entirely with new fees */
   IF lcNewChgMonth = "1" OR lcOldChgMonth = "0" THEN DO:
      ldtFeeDate = DATE(MONTH(ldtActDate),1,YEAR(ldtActDate)). 
   END. 
 
   /* change month entirely with old fees */
   ELSE IF lcOldChgMonth = "1" THEN DO:
      /* if change is done on 1st day and before given time limit,
          then fees for change month will be taken from new type */
      IF DAY(ldtActDate) = 1 AND liTimeLimit >= liActTime
      THEN ldtFeeDate = ldtActDate.

      /* otherwise use fees from old type */
      ELSE DO:
         IF MONTH(ldtActDate) = 12 
         THEN ldtFeeDate = DATE(1,1,YEAR(ldtActDate) + 1). 
         ELSE ldtFeeDate = DATE(MONTH(ldtActDate) + 1,1,YEAR(ldtActDate)).
      END.   
   END.

   /* if not either then check if broken month is not allowed
      -> use old fee for change month */
   ELSE IF lcOldBroken = "0" OR lcNewBroken = "0" THEN DO:
      IF MONTH(ldtActDate) = 12 
      THEN ldtFeeDate = DATE(1,1,YEAR(ldtActDate) + 1). 
      ELSE ldtFeeDate = DATE(MONTH(ldtActDate) + 1,1,YEAR(ldtActDate)).
   END. 

   /* if no restrictions then cut can be made from actual change date */
   ELSE ldtFeeDate = ldtActDate.

   IF lcOldBillItem NE "" THEN 
      RUN pDelFixedFee("",
                       lcOldBillItem,
                       ldtFeeDate,
                       ?,
                       TRUE,  /* clean credit fees also */
                       FALSE,  /* credit singlefee for billed items */
                       Msrequest.UserCode,
                       "STC", /* fee memo */
                       OUTPUT ldReqAmt,
                       OUTPUT liCredQty).

   /* YDA-380 - If PayType is changed then only delete and add new
      service(s) corresponding to new subscription type */

   IF bOldType.PayType <> CLIType.PayType THEN DO:

      /* add services of new type that didn't exist on old type */
      RUN pCopyPackage(MsRequest.ReqCParam2,
                       "*",
                       "",
                       MsRequest.MSSeq,
                       ldtActDate,
                       TRUE,    /* only those that don't already exist */
                       FALSE,   /* create fees */
                       FALSE,   /* solog (this is used for new mobsubs only) */
                       0,
                       FALSE,
                       OUTPUT liReqCnt).

      RUN pCopyPackage(MsRequest.ReqCParam2,
                       "TMSService",
                       "",
                       MsRequest.MSSeq,
                       ldtActDate,
                       TRUE,    /* only those that don't already exist */
                       FALSE,   /* create fees */
                       FALSE,   /* solog (this is used for new mobsubs only) */
                       0,
                       FALSE,
                       OUTPUT liReqCnt).
   END. /* IF bOldType.PayType <> CLIType.PayType THEN DO: */

   /* Close promotion fat */
   IF NOT llOldPayType THEN DO:
      ASSIGN ldaPrevMonth = (DATE(MONTH(ldtActDate),1,YEAR(ldtActDate)) - 1)
             liPrevPeriod = (YEAR(ldaPrevMonth) * 100 + MONTH(ldaPrevMonth)).

      IF MsRequest.ReqCParam1 EQ "CONT5" THEN
         fCloseFat("BONO8CPFREE",
                   MsRequest.MsSeq,
                   liPrevPeriod).

      /* YOT-1557 - Cancel CONSMINFAT */
      fCloseFat("CONSMINFAT",
                MsRequest.MsSeq,
                liPrevPeriod).
   END. /* IF NOT llOldPayType THEN DO: */
   
   /* close discounts that are not allowed for the new type */
   FOR EACH DPMember NO-LOCK WHERE
            DPMember.HostTable = "MobSub" AND
            DPMember.KeyValue = STRING(MsRequest.MsSeq) AND
            DPMember.ValidTo >= ldtActDate,
      FIRST DiscountPlan OF DPMember NO-LOCK WHERE
            DiscountPlan.SubjectType = "List":
     /* NOT REQUIRED ?
      IF CAN-FIND(FIRST DPSubject WHERE 
                        DPSubject.DPId      = DiscountPlan.DPId AND
                        DPSubject.DPSubject MATCHES "CONT*"     AND
                        DPSubject.ValidFrom <= ldtActDate       AND
                        DPSubject.ValidTo   >= ldtActDate) THEN NEXT.
      */                     
      IF NOT CAN-FIND(FIRST DPSubject WHERE 
                            DPSubject.DPId      = DiscountPlan.DPId AND
                            MsRequest.ReqCParam2 MATCHES DPSubject.DPSubject AND
                            DPSubject.ValidFrom <= ldtActDate AND
                            DPSubject.ValidTo   >= ldtActDate) 
      THEN DO TRANS:
         /* Log dpmember modification */
         IF llDoEvent THEN DO:
            lhDPMember = BUFFER DPMember:HANDLE.
            RUN StarEventInitialize(lhDPMember).
            RUN StarEventSetOldBuffer(lhDPMember).
         END.
         FIND FIRST bMember WHERE RECID(bMember) = RECID(DPMember) 
            EXCLUSIVE-LOCK.
         bMember.ValidTo = ldtActDate - 1. 

         /* Additional Line with mobile only ALFMO-5 
             Added {&ADDLINE_DISCOUNTS_HM} */
         IF llAddLineDisc = FALSE AND
            LOOKUP(DiscountPlan.DPRuleID,
                   {&ADDLINE_DISCOUNTS} + "," + 
                   {&ADDLINE_DISCOUNTS_20} + "," + 
                   {&ADDLINE_DISCOUNTS_HM}) > 0
            THEN ASSIGN llAddLineDisc = TRUE
                        lcAddLineDisc = DiscountPlan.DPRuleID.
         
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPMember).

      END.   
   END.
 
   /* ADDLINE-20 Additional Line Discounts 
      CHANGE: If New CLIType Matches, Then Change the Discount accordingly to the new type 
      ADDLINE-267 Phase 2 fix */

   IF llAddLineDisc AND
      LOOKUP(CLIType.CliType , {&ADDLINE_CLITYPES}) > 0 AND
      LOOKUP(bOldType.CliType, {&ADDLINE_CLITYPES}) > 0 THEN DO:
      
      IF lcAddLineDisc = ENTRY(LOOKUP(bOLDType.CLIType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS}) AND
         fCheckExistingConvergent(Customer.CustIDType,Customer.OrgID,CLIType.CLIType) THEN DO:
         fCreateAddLineDiscount(MsRequest.MsSeq,
                                CLIType.CLIType,
                                ldtActDate,
                                ENTRY(LOOKUP(CLIType.CLIType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS})).
         IF RETURN-VALUE BEGINS "ERROR" THEN
            RETURN RETURN-VALUE.
      END.

      IF lcAddLineDisc = ENTRY(LOOKUP(bOLDType.CLIType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_20}) AND
         fCheckExisting2PConvergent(Customer.CustIDType,Customer.OrgID,CLIType.CLIType) THEN DO:
         fCreateAddLineDiscount(MsRequest.MsSeq,
                                CLIType.CLIType,
                                ldtActDate,
                                ENTRY(LOOKUP(CLIType.CLIType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_20})).
         IF RETURN-VALUE BEGINS "ERROR" THEN
            RETURN RETURN-VALUE.
      END.
    
      /* Additional Line with mobile only ALFMO-5 */
      IF lcAddLineDisc = ENTRY(LOOKUP(bOLDType.CLIType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_HM}) AND
         fCheckExistingMobileOnly(Customer.CustIDType,Customer.OrgID,CLIType.CLIType) THEN DO:
         fCreateAddLineDiscount(MsRequest.MsSeq,
                                CLIType.CLIType,
                                ldtActDate,
                                ENTRY(LOOKUP(CLIType.CLIType,{&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_HM})).
         IF RETURN-VALUE BEGINS "ERROR" THEN
            RETURN RETURN-VALUE.
      END.
   END.
   
END PROCEDURE.

PROCEDURE pUpdateSubscription:

   DEF VAR liBillTarg              AS INT  NO-UNDO. 
   DEF VAR liChangeTime            AS INT  NO-UNDO.
   DEF VAR lcFusionSubsType        AS CHAR NO-UNDO.
   DEF VAR ldEndStamp              AS DEC  NO-UNDO.
   DEF VAR ldBegStamp              AS DEC  NO-UNDO.
   DEF VAR liManTime               AS INT  NO-UNDO.
   DEF VAR lcMandateId             AS CHAR NO-UNDO. 
   DEF VAR ldaMandateDate          AS DATE NO-UNDO. 
   DEF VAR llUpdateMandate         AS LOG  NO-UNDO. 
   DEF VAR ldeNewBeginTs           AS DEC  NO-UNDO. 
   DEF VAR ldeOrigTsEnd            AS DEC  NO-UNDO. 
   DEF VAR liLoop                  AS INT  NO-UNDO. 
   DEF VAR lcFixedNumber           AS CHAR NO-UNDO. 
   DEF VAR liSecs                  AS INT  NO-UNDO. 
   DEF VAR liNewMSStatus           AS INT  NO-UNDO. 
   DEF VAR ldtCloseDate            AS DATE NO-UNDO.
   DEF VAR liRequest               AS INT  NO-UNDO.
   DEF VAR lcExtraLineDiscount     AS CHAR NO-UNDO.
   DEF VAR liELCount               AS INT  NO-UNDO.
   DEF VAR liMLMsSeq               AS INT  NO-UNDO.
   DEF VAR lcExtralineCLITypes     AS CHAR NO-UNDO.
   DEF VAR liELCLITypeCount        AS INT  NO-UNDO.
   DEF VAR llgConfigExtraLineCount AS LOG  NO-UNDO.
   DEF VAR liConfigExtraLineCount  AS INT  NO-UNDO.
   DEF VAR lcMandatoryExtraLines   AS CHAR NO-UNDO.
   DEF VAR lcAllowedExtraLines     AS CHAR NO-UNDO.   
   DEF VAR liAllowedELCount        AS INT  NO-UNDO.
   DEF VAR liManELCount            AS INT  NO-UNDO.
   DEF VAR liAvailExtraLineCount   AS INT  NO-UNDO.
   DEF VAR llgMandatoryExtraLine   AS LOG  NO-UNDO.
   DEF VAR llgDSSBundle            AS LOG  NO-UNDO.     
   DEF VAR lcAssignSubId           AS CHAR NO-UNDO.
   DEF VAR liAssignSubId           AS INT  NO-UNDO.
   DEF VAR liOrigMsSeq             AS INT  NO-UNDO.
   
   DEF BUFFER bOwner         FOR MsOwner.
   DEF BUFFER bMobSub        FOR MobSub.
   DEF BUFFER lbELMobSub     FOR MobSub.
   DEF BUFFER ELMobSub2      FOR MobSub.  
   DEF BUFFER lMLMobSub      FOR MobSub.
   DEF BUFFER lbDiscountPlan FOR DiscountPlan.
   DEF BUFFER lbDPMember     FOR DPMember.
   DEF BUFFER lbOrigRequest  FOR MsRequest.

   /* make sure that customer has a billtarget with correct rateplan */
   liBillTarg = CLIType.BillTarget.
   
   FIND BillTarg NO-LOCK WHERE 
        BillTarg.CustNum    = Mobsub.CustNum AND
        BillTarg.BillTarget = liBillTarg NO-ERROR.

   IF NOT AVAILABLE BillTarg THEN liBillTarg = 0.
   /* Check configuration error with Rateplan and Priceplan */
   ELSE IF BillTarg.RatePlan NE CLIType.PricePlan THEN liBillTarg = -1.

   IF liBillTarg <= 0 THEN DO:
   
      /* check if some other billtarget has the same rateplan */
      FOR FIRST BillTarg NO-LOCK WHERE
                BillTarg.CustNum  = Mobsub.CustNum AND
                BillTarg.RatePlan = CLIType.PricePlan:
         liBillTarg = BillTarg.BillTarget.       
      END.
   
      IF liBillTarg <= 0 THEN DO:
         
         IF liBillTarg < 0 THEN DO:
            FIND LAST BillTarg WHERE BillTarg.CustNum = Mobsub.CustNum         
               NO-LOCK NO-ERROR.
            /* Create new billtarget for this case. 
               Error can be corrected afterwards YTS-8271 */
            IF AVAILABLE BillTarg THEN DO:
               IF BillTarg.BillTarget < 1000 THEN
                   liBillTarg = 1000.
               ELSE liBillTarg = BillTarg.BillTarget + 1.
            END.
            ELSE liBillTarg = 1.
         END.
         ELSE liBillTarg = CLIType.BillTarget.
         
         CREATE BillTarg.
         ASSIGN
            BillTarg.CustNum    = Customer.CustNum
            BillTarg.BillTarget = liBillTarg
            BillTarg.DiscPlan   = CLIType.DiscPlan
            BillTarg.RatePlan   = CLIType.PricePlan.
      END. 
   END.

   /* Create Mandate for Subscription and store it into MsOwner */

   IF CLIType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID} THEN ASSIGN
      lcMandateId     = ""
      ldaMandateDate  = ?
      llUpdateMandate = TRUE.
   ELSE IF CLIType.PayType = {&CLITYPE_PAYTYPE_POSTPAID} AND
      bOldType.Paytype = {&CLITYPE_PAYTYPE_PREPAID} THEN DO:
      Func.Common:mSplitTS(MsRequest.CreStamp, OUTPUT ldaMandateDate, OUTPUT liManTime).
      fCalculateMandate(MobSub.MsSeq,
                        ldaMandateDate,
                        MsRequest.CustNum, 
                        OUTPUT lcMandateId).
      llUpdateMandate = TRUE.
   END.
  
   IF MsRequest.ReqIParam2 > 0 AND
      CAN-FIND(FIRST Order NO-LOCK WHERE
                     Order.Brand = Syst.Var:gcBrand AND
                     Order.OrderID = MsRequest.ReqIParam2 AND
                     Order.OrderType EQ {&ORDER_TYPE_STC}) THEN DO:

      FIND FIRST Order NO-LOCK WHERE
                 Order.Brand = Syst.Var:gcBrand AND
                 Order.OrderID = MsRequest.ReqIParam2 AND
                 Order.OrderType EQ {&ORDER_TYPE_STC}.

      fGetOrderMandateId(BUFFER Order,
                         OUTPUT lcMandateId,
                         OUTPUT ldaMandateDate).
      llUpdateMandate = TRUE.
   END.
   
   /* owner history; mark end time to current one and create a new
      one starting from change time */
   ASSIGN 
      ldBegStamp = MsRequest.ActStamp
      ldEndStamp = Func.Common:mSecOffSet(ldBegStamp,-1)
      lcFixedNumber = ?.
   
   IF MsRequest.ReqIParam2 > 0 THEN
      FOR FIRST Order NO-LOCK WHERE
                Order.Brand = Syst.Var:gcBrand AND
                Order.OrderID = MsRequest.ReqIparam2,
          FIRST OrderFusion NO-LOCK WHERE
                OrderFusion.Brand = Order.Brand AND
                OrderFusion.OrderID = Order.OrderID:
         lcFixedNumber = OrderFusion.FixedNumber.
      END.
   
   /* YTS-10293 */
   IF fisConvergenceTariff(MsRequest.reqcparam1) AND
      fisConvergenceTariff(MsRequest.reqcparam2) AND
      lcFixedNumber EQ ? THEN
      lcFixedNumber = mobsub.fixednumber.

   /* YOT-1407: if postpaid -> postpaid or postpaid -> prepaid then original 
      activation time can be used even if handling is delayed */
   IF llOldPayType = FALSE OR 
      lcFixedNumber NE ? OR
      ldtActDate > TODAY THEN ASSIGN 
      ldaNewBeginDate = ldtActDate
      liChangeTime    = liActTime.
   ELSE ASSIGN
      ldaNewBeginDate = TODAY
      liChangeTime    = TIME.   

   ldeNewBeginTs = Func.Common:mMake2DT(ldaNewBeginDate, liChangeTime).
   
   FOR EACH MSOwner WHERE 
            MSOwner.MsSeq  = Mobsub.MsSeq    AND
            MSOwner.TsEnd >= ldeNewBeginTs
   EXCLUSIVE-LOCK BY MSOwner.TsEnd:

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsOwner).

      liLoop = liLoop + 1.
         
      IF liLoop EQ 1 THEN DO:

         /* YOB-1145 */
         liSecs = -1.
         DO WHILE TRUE:
            IF NOT CAN-FIND(FIRST bOwner WHERE
                       bOwner.CLI = MSOwner.CLI AND
                       bOwner.TSEnd = Func.Common:mSecOffSet(ldeNewBeginTs, liSecs))
               THEN LEAVE.
            liSecs = liSecs - 1.
         END.
         
         ASSIGN
            ldeOrigTsEnd = MSOwner.TsEnd
            MSOwner.TsEnd = Func.Common:mSecOffSet(ldeNewBeginTs, liSecs).
         
         CREATE bOwner.
         BUFFER-COPY MsOwner EXCEPT TsBegin TsEnd CLIEvent TO bOwner.
         ASSIGN bOwner.CLIType    = MsRequest.ReqCParam2
                bOwner.TsBegin    = ldeNewBeginTs
                bOwner.TsEnd      = ldeOrigTsEnd
                bOwner.BillTarget = liBillTarg
                bOwner.Paytype    = (CLIType.PayType = 2)
                bOwner.MandateId   = lcMandateId WHEN llUpdateMandate
                bOwner.MandateDate = ldaMandateDate WHEN llUpdateMandate
                ldEndStamp        = MsOwner.TSEnd
                ldBegStamp        = bOwner.TSBeg
                bOwner.TariffBundle = MsRequest.ReqCParam5
                bOwner.FixedNumber = lcFixedNumber.
      
         IF DAY(ldtActDate) <> 1 THEN DO:
            lcFusionSubsType = fCParamC("FUSION_SUBS_TYPE").

           /* Immediate STC Invoice Split, fusion to non-fusion or vice-versa */
            IF (LOOKUP(MsRequest.ReqCParam1,lcFusionSubsType) > 0 AND
                LOOKUP(MsRequest.ReqCParam2,lcFusionSubsType) = 0) OR
               (LOOKUP(MsRequest.ReqCParam1,lcFusionSubsType) = 0 AND
                LOOKUP(MsRequest.ReqCParam2,lcFusionSubsType) > 0) THEN
               bOwner.CLIEvent = "iSS".
           /* Immediate STC without Invoice Split, pos=>pos or fusion=>fusion */
            ELSE
               bOwner.CLIEvent = "iS".
         END.
         ELSE bOwner.CLIEvent = "S".

         IF llDoEvent THEN fMakeCreateEvent((BUFFER bOwner:HANDLE),
                                            "",
                                            Syst.Var:katun,
                                            "").
         RELEASE bOwner.
      END.
      ELSE ASSIGN MSOwner.CLIType    = MsRequest.ReqCParam2
                  MsOwner.BillTarget = liBillTarg
                  MsOwner.Paytype    = (CLIType.PayType = 2)
                  MsOwner.MandateId   = lcMandateId WHEN llUpdateMandate
                  MsOwner.MandateDate = ldaMandateDate WHEN llUpdateMandate
                  MsOwner.TariffBundle = MsRequest.ReqCParam5
                  MsOwner.FixedNumber = lcFixedNumber.

      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsOwner).
   END.
   
   liNewMSStatus = Mobsub.MsStatus.

   IF Mobsub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} THEN DO:
      IF bNewTariff.TariffType EQ {&CLITYPE_TARIFFTYPE_FIXEDONLY} THEN   
         liNewMSStatus = {&MSSTATUS_MOBILE_NOT_ACTIVE}.
      ELSE IF bNewTariff.TariffType NE {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
         liNewMSStatus = {&MSSTATUS_ACTIVE}.
   END.

   /* update subscription */
   FIND CURRENT Mobsub EXCLUSIVE-LOCK.

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobsub).

   ASSIGN Mobsub.CLIType       = MsRequest.ReqCParam2
          Mobsub.BillTarget    = liBillTarg
          Mobsub.Paytype       = (CLIType.PayType = 2)
          Mobsub.TariffActDate = ldtActDate
          MobSub.TariffActTS   = ldeNewBeginTs
          MobSub.FixedNumber   = lcFixedNumber
          MobSub.CLI           = MobSub.FixedNumber WHEN
                                 MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} AND
                                 liNewMSStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}
          MobSub.MsStatus      = liNewMSStatus.

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobsub).
   
   FIND CURRENT Mobsub NO-LOCK.

   /* Open the BB Fixed fee if subs. is migrating from PRE->POST */
   IF bOldType.PayType = 2 AND CLIType.PayType = 1 THEN
      FOR FIRST SubSer WHERE
                SubSer.ServCom = "BB" AND
                SubSer.MsSeq   = MsRequest.MsSeq AND
                SubSer.SsDate <= TODAY NO-LOCK,
          FIRST ServCom NO-LOCK WHERE
                ServCom.Brand   = Syst.Var:gcBrand AND
                ServCom.ServCom = SubSer.ServCom:
         IF SubSer.SSStat = 1 THEN
            fServiceOpenFee(ServCom.FeeModel,
                            ldtActDate,
                            "",
                            "",
                            MsRequest.UserCode,
                            "STC").
      END. /* FOR FIRST SubSer WHERE */

   /*       Extraline STC Cases        */
   /* 1. STC To Extraline              */
   /* 2. STC To Non Mainline/Extraline */
   /* 3. STC To Mainline               */
   IF fCLITypeIsExtraLine(CLIType.CliType) THEN DO:

      IF fCheckMsRequestParam(MsRequest.MsRequest,
                              {&EXTRALINE_STC},
                              OUTPUT lcAssignSubId) THEN DO:

         FIND FIRST lbOrigRequest NO-LOCK WHERE
                    lbOrigRequest.Brand     EQ Syst.Var:gcBrand                    AND
                    lbOrigRequest.ReqType   EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                    lbOrigRequest.MsRequest EQ MsRequest.OrigRequest               NO-ERROR.

         IF AVAIL lbOrigRequest THEN
            liOrigMsSeq = lbOrigRequest.MsSeq. 
      END. 

      IF liOrigMsSeq GT 0 THEN DO:
         fResetExtralineSubscription(MobSub.MsSeq,
                                     "",
                                     liOrigMsSeq,
                                     {&MULTISIMTYPE_EXTRALINE},
                                     TRUE).
         DO liAssignSubId = 1 TO NUM-ENTRIES(lcAssignSubId,CHR(255)):
            FIND FIRST lbELMobSub NO-LOCK WHERE
                       lbELMobSub.MsSeq        EQ INT(ENTRY(liAssignSubId,lcAssignSubId,CHR(255))) AND
                       lbELMobSub.MultiSimId   EQ 0                                                AND
                       lbELMobSub.MultiSimType EQ 0                                                AND
                       lbELMobSub.CustNum      EQ MobSub.CustNum                                   AND
                      (lbELMobSub.MsStatus EQ {&MSSTATUS_ACTIVE} OR
                       lbELMobSub.MsStatus EQ {&MSSTATUS_BARRED})                                  NO-ERROR.
            IF AVAIL lbELMobSub THEN DO:
               fResetExtralineSubscription(lbELMobSub.MsSeq,
                                           "",
                                           liOrigMsSeq,
                                           {&MULTISIMTYPE_EXTRALINE},
                                           TRUE).
               fUpdateDSSAccount(lbELMobSub.MsSeq,
                                 {&REQUEST_SOURCE_STC},
                                 MsRequest.MsRequest,
                                 Func.Common:mMakeTS(),
                                 "CREATE").
            END.
         END.
      END.
      ELSE DO:
         fCheckExistingMainLineAvailForExtraLine(INPUT MobSub.CLIType,
                                                 INPUT Customer.CustIdType,
                                                 INPUT Customer.OrgID,
                                                 OUTPUT liMLMsSeq).
         IF liMLMsSeq > 0 THEN  
            fResetExtralineSubscription(MobSub.MsSeq,
                                        "",
                                        liMLMsSeq,
                                        {&MULTISIMTYPE_EXTRALINE},
                                        TRUE).                        
      END.
   END.
   ELSE IF fCLITypeIsMainLine(CLIType.CliType) THEN DO:

      fUpdateDSSAccount(MobSub.MsSeq,
                        {&REQUEST_SOURCE_STC},
                        MsRequest.MsRequest,
                        Func.Common:mMakeTS(),
                        "DELETE").

      /* If old clitype is mainline then reset all associated extralines multisim values */
      /* If old clitype is extraline then reset its multisim values                      */
      IF fCLITypeIsMainLine(bOldType.CliType) THEN DO:

         FOR EACH lbELMobSub NO-LOCK WHERE
                  lbELMobSub.Brand        EQ Syst.Var:gcBrand AND
                  lbELMobSub.MultiSimId   EQ MobSub.MsSeq     AND
                  lbELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:
            fResetExtralineSubscription(lbELMobSub.MsSeq,
                                        lbELMobSub.CLIType,
                                        0,
                                        0,
                                        FALSE).
         END.

      END.
      ELSE IF fCLITypeIsExtraLine(bOldType.CliType) THEN
         fResetExtralineSubscription(MobSub.MsSeq,
                                     CLIType.CLIType,
                                     0,
                                     0,
                                     FALSE).

      /* Check for available extralines of the customer and     */
      /* then reassign them to new mainline and create discount */
      IF CAN-FIND(FIRST ttExtraLines NO-LOCK) THEN
         IF fReassigningExtralines(MobSub.MsSeq,
                                   {&REQUEST_SOURCE_STC},
                                   MsRequest.MsRequest) THEN
            fCheckAndAssignOrphanExtraline(MobSub.MsSeq,
                                           MobSub.CustNum,
                                           MobSub.CLIType).
   END.

   /* ADDLINE-324 Additional Line Discounts
      CHANGE: If STC happened on convergent, AND the customer does not have any other fully convergent
      then CLOSE the all addline discounts to (STC Date - 1) */

   /* Mobile only additional line ALFMO-5 */
   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Var:gcBrand AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(bOldType.CliType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.

   IF fIsConvergenceTariff(bOldType.CliType) AND 
      NOT fCheckExistingConvergent(Customer.CustIDType,Customer.OrgID,CLIType.CLIType)     THEN DO:
      FOR EACH bMobSub NO-LOCK WHERE
               bMobSub.Brand   = Syst.Var:gcBrand          AND
               bMobSub.AgrCust = Customer.CustNum AND
               bMobSub.MsSeq  <> MsRequest.MsSeq  AND
               LOOKUP(bMobSub.CliType, {&ADDLINE_CLITYPES}) > 0:
         fCloseAddLineDiscount(bMobSub.CustNum,
                               bMobSub.MsSeq,
                               bMobSub.CLIType,
                               IF MONTH(bMobSub.ActivationDate) = MONTH(TODAY) THEN Func.Common:mLastDayOfMonth(TODAY)
                               ELSE ldtActDate - 1).
         
      END.
   END.
   /* YPRO. If fixedline is terminated from convergent, also SVAs should be
      terminated. */
   IF fIsConvergenceTariff(bOldType.CliType) AND 
      NOT fIsConvergenceTariff(CLIType.CliType) THEN DO:
      fTerminateSVAs(Mobsub.msseq, FALSE).
      FIND FIRST MSRequest WHERE
                 MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.
      fDeactivateTVService(Mobsub.MsSeq, MsRequest.UserCode).
      FIND MsRequest NO-LOCK WHERE
           MsRequest.MsRequest = iiMSRequest.
   END.

   /* Additional Line with mobile only ALFMO-5 
      IF STC happened and the new main line is not mobile only or matrix doesn't meet
      then close the additional line discount */
   ELSE IF AVAIL DiscountPlan AND 
      bOldType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY} AND
      NOT CAN-FIND(FIRST DPMember WHERE
                   DPMember.DPId      = DiscountPlan.DPId AND
                   DPMember.HostTable = "MobSub" AND
                   DPMember.KeyValue  = STRING(MsRequest.MsSeq) AND
                   DPMember.ValidTo   <> 12/31/49) THEN
   DO:
      FOR EACH bMobSub NO-LOCK WHERE
               bMobSub.Brand   = Syst.Var:gcBrand          AND
               bMobSub.AgrCust = Customer.CustNum AND
               bMobSub.MsSeq  <> MsRequest.MsSeq  AND
               LOOKUP(bMobSub.CliType, {&ADDLINE_CLITYPES}) > 0:

         /* Additional Line with mobile only ALFMO-5 */
         IF MONTH(bMobSub.ActivationDate) = MONTH(TODAY) AND 
            YEAR(bMobSub.ActivationDate) = YEAR(TODAY) THEN
            ASSIGN ldtCloseDate = Func.Common:mLastDayOfMonth(TODAY).
         ELSE IF MONTH(bMobSub.ActivationDate) < MONTH(TODAY) OR
            YEAR(bMobSub.ActivationDate) < YEAR(TODAY) THEN
            ASSIGN ldtCloseDate = ldtActDate - 1.

         fCloseAddLineDiscount(bMobSub.CustNum,
                               bMobSub.MsSeq,
                               bMobSub.CLIType,
                               ldtCloseDate).               
   
      END.
   END.

END PROCEDURE.


PROCEDURE pFinalize:

   DEF VAR liChargeReqId         AS INT  NO-UNDO.
   DEF VAR liFatFromPeriod       AS INT  NO-UNDO. 
   DEF VAR ldtDate               AS DATE NO-UNDO. 
   DEF VAR liTime                AS INT  NO-UNDO.
   DEF VAR lcCharValue           AS CHAR NO-UNDO. 
   DEF VAR liRequest             AS INT  NO-UNDO.
   DEF VAR liCustnum             AS INT  NO-UNDO. 
   DEF VAR ldEndStamp            AS DEC  NO-UNDO.
   DEF VAR ldBegStamp            AS DEC  NO-UNDO.
   DEF VAR ldeNow                AS DEC  NO-UNDO.
   DEF VAR lcResult              AS CHAR NO-UNDO.
   DEF VAR lcError               AS CHAR NO-UNDO.
   DEF VAR lcMultiLineSubsType   AS CHAR NO-UNDO.
   DEF VAR lcFusionSubsType      AS CHAR NO-UNDO.
   DEF VAR lcPostpaidDataBundles AS CHAR NO-UNDO.
   DEF VAR lcDataBundleCLITypes  AS CHAR NO-UNDO.
   DEF VAR llMigrationNeeded     AS LOG  NO-UNDO.
   DEF VAR lcParamValue          AS CHAR NO-UNDO.
   DEF VAR liMSISDNStat          AS INT  NO-UNDO.
   DEF VAR liSIMStat             AS INT  NO-UNDO.
   DEF VAR liQuarTime            AS INT  NO-UNDO.
   DEF VAR llPenalty             AS LOG  NO-UNDO.
   DEF VAR liTermReq             AS INT  NO-UNDO.
   DEF VAR ocResult              AS CHAR NO-UNDO.
   DEF VAR liMergeOrderId        AS INT  NO-UNDO.
   DEF VAR lcTry&BuyCliTypes     AS CHAR NO-UNDO.
    
   DEF BUFFER DataContractReq FOR MsRequest. 
   DEF BUFFER Order           FOR Order.
   DEF BUFFER bMobsub         FOR Mobsub.
   DEF BUFFER bCustomer       FOR Customer.
   DEF BUFFER bMergeOrder     FOR Order.
   DEF BUFFER bufMergeOrder   FOR Order.
   DEF BUFFER bMergeMsOwner   FOR MsOwner.
   DEF BUFFER bufMergeMsOwner FOR MsOwner.
   DEF BUFFER lbELMobSub      FOR MobSub.
   DEF BUFFER lbCustomer      FOR Customer.

   ASSIGN 
      lcTry&BuyCliTypes    = fCParamC("Try&BuyCliTypes").   
   /* now when billtarget has been updated new fees can be created */

   FIND FIRST MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.

   FIND FIRST MSOwner WHERE
              MSOwner.MsSeq   = Mobsub.MsSeq AND
              MSOwner.TsBeg   = Mobsub.TariffActTS NO-LOCK NO-ERROR.
   
   IF NOT AVAIL MSOwner THEN
   FIND FIRST MSOwner WHERE
              MSOwner.MsSeq   = Mobsub.MsSeq AND
              MSOwner.TsEnd  >= 99999999 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN DO:
      fReqError("Timestamp history missing").
      RETURN.
   END.

   ASSIGN
      ldBegStamp = MsOwner.TSBeg
      ldEndStamp = Func.Common:mSecOffSet(MsOwner.TsBeg,-1)
      /* some time has already passed from subscription update */
      ldeNow     = Func.Common:mMakeTS().
         
   Func.Common:mSplitTS(MsOwner.TsBeg,
            OUTPUT ldaNewBeginDate,
            OUTPUT liTime).

   /* clitype spesific fees */
   IF AVAIL CliType AND CliType.FeeModel1 > "" THEN DO:
      RUN Mc/creasfee.p (MobSub.CustNum,
                    MobSub.MsSeq,
                    ldaNewBeginDate,
                    "MobSub",
                    CliType.FeeModel1,
                    1,
                    ?,
                    "",    /* memo   */
                    FALSE,           /* no messages to screen */
                    MsRequest.UserCode,
                    "STC",
                    0,
                    "",
                    "",
                    OUTPUT lcCharValue).
   END.

   /* general fees */
   RUN Mc/creasfee.p (MobSub.CustNum,
                 MobSub.MsSeq,
                 ldaNewBeginDate,
                 "MobSub",
                 "MONTHLYFEE",
                 1,
                 ?,
                 "",              /* memo   */
                 FALSE,           /* no messages to screen */
                 MsRequest.UserCode,
                 "STC",
                 0,
                 "",
                 "",
                 OUTPUT lcCharValue).

   /* default counter limits */
   IF MobSub.PayType = FALSE THEN DO:
      fTMRLimit2Subscription(MobSub.MsSeq).
      fSetSpecialTTFLimit(MobSub.Custnum,
                          MobSub.CLIType).
   END.

   /* commission termination */
   IF llOldPayType NE MobSub.PayType THEN 
      RUN Ar/commission_term.p(MobSub.MsSeq,
                          "STC",
                          OUTPUT liReqCnt).

   FIND FIRST Order NO-LOCK WHERE
      Order.Brand = Syst.Var:gcBrand AND
      Order.OrderID = MsRequest.ReqIParam2 AND
      Order.OrderType EQ {&ORDER_TYPE_STC} NO-ERROR.

    /* activate/terminate periodical contracts, service packages etc. */
   RUN Mm/requestaction_exec.p (MsRequest.MsRequest,
                               MsRequest.ReqCParam2, /* definitions on new type */
                               (IF AVAILABLE Order THEN Order.OrderId ELSE 0), /* order */
                               ldBegStamp,
                               ldEndStamp,
                               TRUE,                   /* create fees */
                               {&REQUEST_SOURCE_STC},  /* req.source */
                               {&REQUEST_ACTIONLIST_ALL}).

    /* Create charge for new paytype */
    IF MsRequest.CreateFees THEN 
        RUN Mm/create_charge_comp.p(
           {&REQUEST_SOURCE_MANUAL_TMS},
           Mobsub.MsSeq,   
           MsRequest.UserCode,
           MsRequest.ReqDParam2,  
           "STC_" + (IF Mobsub.PayType THEN "PREPAID" ELSE "POSTPAID"),
           MsRequest.MsRequest, 
           OUTPUT liChargeReqId) NO-ERROR.

   /* YTS-8159 */
    IF bOldType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID} AND
       bOldType.CLIType EQ "TARJ6"                    AND
       CLIType.PayType  EQ {&CLITYPE_PAYTYPE_PREPAID} AND
       LOOKUP(CLIType.CLIType,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN DO:

       FIND FIRST ServiceLimit NO-LOCK WHERE
                  ServiceLimit.Groupcode EQ CLIType.CLIType AND
                  ServiceLimit.DialType  EQ 7               NO-ERROR.

       IF AVAIL ServiceLimit AND
          NOT CAN-FIND(FIRST DataContractReq NO-LOCK WHERE
                             DataContractReq.MsSeq      = MobSub.MsSeq                   AND
                             DataContractReq.ActStamp  >= ldeActStamp                    AND
                             DataContractReq.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND
                             DataContractReq.ReqCparam3 = CLIType.CLIType)               AND
          NOT CAN-FIND(FIRST MServiceLimit NO-LOCK WHERE
                             MServiceLimit.MsSeq    = MobSub.MsSeq          AND
                             MServiceLimit.SLSeq    = ServiceLimit.SlSeq    AND
                             MserviceLimit.DialType = ServiceLimit.DialType AND
                             MserviceLimit.EndTS   >= ldeNow                AND
                             MserviceLimit.FromTs  <= ldeNow) THEN DO:

          liRequest = fServiceRequest(MobSub.MsSeq,
                                      "SHAPER",
                                      1,
                                      "DEFAULT",
                                      ldeNow,
                                      "",
                                      FALSE,
                                      FALSE,
                                      "",
                                      {&REQUEST_SOURCE_STC},
                                      MsRequest.MsRequest,
                                      FALSE,
                                      OUTPUT lcResult).
          IF liRequest = 0 THEN
            Func.Common:mWriteMemo("MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.CustNum,
                       "ERROR:DEFAULT SHAPER request creation failed",
                       lcResult).
       END.
    END.

   /* RUN Rate/rerate.p (needed especially with saldo-services) */
   IF (bOldType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID} OR
       CLIType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID}) THEN DO:
      
      RUN pReRate(MobSub.MsSeq,
                  MobSub.InvCust,
                  ldaNewBeginDate).
   END.
   /* YOT-627 */
   IF bOldType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID} AND
       CLIType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID} AND
       MsRequest.ReqIParam3 > 0 THEN DO:

      liFatFromPeriod = YEAR(ldaNewBeginDate) * 100 + MONTH(ldaNewBeginDate).
    
      /* create FAtime */
      RUN Mc/creafat.p (MobSub.CustNum,
                     MobSub.MsSeq,
                     "BTPREPOST1000",
                     (MsRequest.ReqIParam3 / 100),
                     0,
                     ?,
                     liFATFromPeriod,
                     999999,
                     OUTPUT lcError).
                     
      IF lcError > "" THEN
         Func.Common:mWriteMemo("MobSub",
                    STRING(MobSub.MsSeq),
                    MobSub.CustNum,
                    "Subscription type change",
                    "FATime event could not be created: " + lcError).
   END.
           
   IF CLIType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID} THEN DO:

      ASSIGN lcMultiLineSubsType = fCParamC("MULTILINE_SUBS_TYPE")
             lcFusionSubsType    = fCParamC("FUSION_SUBS_TYPE").

      IF bOldType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID} OR
         (LOOKUP(CLIType.CLIType,lcMultiLineSubsType)  > 0 AND
          LOOKUP(bOldType.CLIType,lcMultiLineSubsType) = 0) OR
         (LOOKUP(CLIType.CLIType,lcMultiLineSubsType)  = 0 AND
          LOOKUP(bOldType.CLIType,lcMultiLineSubsType) > 0) OR
         (LOOKUP(CLIType.CLIType,lcFusionSubsType)  > 0 AND
          LOOKUP(bOldType.CLIType,lcFusionSubsType) = 0) OR
         (LOOKUP(CLIType.CLIType,lcFusionSubsType)  = 0 AND
          LOOKUP(bOldType.CLIType,lcFusionSubsType) > 0) THEN DO:
         fSTCInvoiceTarget(MobSub.MsSeq,
                           MsRequest.ReqCParam1,
                           MsRequest.ReqCParam2,
                           OUTPUT lcError).

         IF lcError NE "" THEN 
            Func.Common:mWriteMemo("MobSub",
                             STRING(MobSub.MsSeq),
                             MobSub.CustNum,
                             "Subscription type change",
                             "Invoice target creation failed: " + lcError).
      END.
   END.
      
   IF bOldType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID} AND
      CLIType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID} THEN DO:

      /* Quota 25 q25 - YPR-2521 */
      FOR EACH MSRequest NO-LOCK WHERE  
               MSRequest.MsSeq      EQ Mobsub.MsSeq AND
               MSRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
               MsRequest.ReqStatus  EQ 0 AND
               MSREquest.REqcparam3 EQ "RVTERM12":   
         fReqStatus(4,"Cancelled by STC to prepaid").
      END. /* FOR EACH MSRequest */

      FIND MSRequest WHERE MSRequest.MsRequest = iiMSRequest NO-LOCK.
   END.

   IF MsRequest.ReqIParam2 > 0 THEN DO:
      /* release possible renewal pos stc order */
      FIND FIRST Order NO-LOCK WHERE
         Order.Brand   = Syst.Var:gcBrand AND
         Order.OrderID = MsRequest.ReqIParam2 AND
         Order.StatusCode = {&ORDER_STATUS_RENEWAL_STC} AND
         LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0
         NO-ERROR.
      IF AVAIL Order THEN DO:
         fSetOrderStatus(Order.OrderId, {&ORDER_STATUS_RENEWAL}).
         /* Mark the timestamp as change */
         fMarkOrderStamp(Order.OrderID,"Change",0.0).
      END.
   END.

   /* DSS related activity */
   RUN pUpdateDSSAccount(INPUT MsRequest.MsRequest,
                         INPUT MsRequest.ActStamp,
                         INPUT ldtActDate,
                         INPUT MsRequest.UserCode).

   IF Customer.Language NE 1 AND
      bOldType.PayType NE CLIType.PayType THEN DO:

      liRequest = fServiceRequest(
                     MobSub.MsSeq,
                     "LANG",
                     Customer.Language,
                     "", /* param */
                     Func.Common:mMakeTS(),
                     "", /* salesman */
                     TRUE,      /* fees */
                     FALSE,      /* sms */
                     "", /* usercode */
                     {&REQUEST_SOURCE_STC},
                     msrequest.msrequest, /* father request */
                     false, /* mandatory for father request */
                     OUTPUT lcerror).
      
      IF liRequest = 0 THEN                               
         /* write possible error to a memo */
         Func.Common:mWriteMemo("MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.Custnum,
                          "Voicemail language change failed",
                          lcError).
   END.

   /* Finalize fusion STC order **/
   IF MsRequest.ReqIParam2 > 0 THEN DO:

      FOR FIRST Order NO-LOCK WHERE
                Order.Brand = Syst.Var:gcBrand AND
                Order.OrderID = MsRequest.ReqIParam2 AND
                Order.OrderType EQ {&ORDER_TYPE_STC}:

         IF Order.StatusCode EQ {&ORDER_STATUS_ONGOING} THEN DO:
            FOR FIRST OrderCustomer NO-LOCK WHERE
                      OrderCustomer.brand EQ Syst.Var:gcBrand AND
                      Ordercustomer.orderid EQ MsRequest.ReqIParam2 AND
                      OrderCustomer.rowtype EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} AND
                      Ordercustomer.pro,
                FIRST bCustomer NO-LOCK WHERE
                      bCustomer.brand EQ Syst.Var:gcbrand AND
                      bCustomer.orgid EQ Ordercustomer.custid AND
                      bcustomer.custidtype EQ Ordercustomer.CustIdType AND
                      bcustomer.category NE Ordercustomer.category,
                FIRST bMobsub NO-LOCK WHERE
                      bMobsub.brand EQ Syst.Var:gcbrand AND
                      bMobsub.custnum EQ bcustomer.custnum AND
                      bMobsub.msseq ne MsRequest.msseq:
               llmigrationNeeded = TRUE.
            END.         
            /* update customer data */
            RUN Mm/createcustomer.p(Order.OrderId,1,FALSE,TRUE,output liCustnum).

            /* possible bono/bono voip activation */
            RUN Mm/orderaction_exec.p (MobSub.MsSeq,
                                    Order.OrderID,
                                    ?,
                                    MsRequest.MsRequest,
                                    {&REQUEST_SOURCE_STC}).
            IF llMigrationNeeded THEN DO:
               lcResult =  fProMigrateOtherSubs (order.Custnum, order.msseq,
                                                 MSRequest.msrequest,
                                                 MSRequest.salesman).
               FIND MSRequest WHERE MSRequest.MsRequest = iiMSRequest NO-LOCK.
            
               IF lcResult > "" THEN DO:
                  /* write possible error to a memo */
                  Func.Common:mWriteMemo("MobSub",
                                   STRING(MobSub.MsSeq),
                                   MobSub.Custnum,
                                   "Migration failed",
                                   lcResult).
               END.
            END.
			
            FIND FIRST OrderCustomer NO-LOCK WHERE
                       OrderCustomer.Brand EQ Syst.Var:gcBrand AND
                       Ordercustomer.OrderID EQ Order.OrderID AND
                       OrderCustomer.rowtype EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL} AND
                       OrderCustomer.TerritoryOwner NE "" NO-ERROR.

				IF Avail OrderCustomer THEN DO:
               FIND CURRENT Mobsub EXCLUSIVE-LOCK NO-ERROR.
                  ASSIGN MobSub.TerritoryOwner = OrderCustomer.TerritoryOwner.
               FIND CURRENT Mobsub NO-LOCK NO-ERROR.				
				END.

            /* YTS-11912 */
            IF fCLITypeIsMainLine(Order.CLIType) THEN  
               fActionOnExtraLineOrders(Order.OrderId,    /* Main line Order Id  */
                                        "RELEASE").       /* Action              */

            fSetOrderStatus(Order.OrderId,"6").  
            fMarkOrderStamp(Order.OrderID,
                            "Delivery",
                            Func.Common:mMakeTS()).
         END.
         ELSE Func.Common:mWriteMemo("MobSub",
              STRING(MobSub.MsSeq),
              MobSub.Custnum,
              "Fusion order finalization failed",
              SUBST("Wrong order status: &1",Order.statusCode)).
      END.
   END.

   /* request handled succesfully */
   fReqStatus(2,"").

   /* YCO-968 */ 
   IF LOOKUP(MsRequest.ReqCparam1, lcTry&BuyCliTypes) > 0 THEN DO:
      lcError = fAddDiscountPlanMember(MsRequest.MsSeq,
                                       "CONT_DISC_TB_20",
                                       16.53, /* discount */
                                       ldtActDate,
                                       12/31/18, 
                                       ?,
                                       0).

      IF RETURN-VALUE BEGINS "ERROR" THEN
         RETURN RETURN-VALUE.
   END.
 
   MERGEREQUEST:
   DO:
      IF fCheckMsRequestParam(MsRequest.MsRequest,
                              {&MERGE2P3P},
                              OUTPUT lcParamValue) THEN DO:

         FIND CURRENT Mobsub EXCLUSIVE-LOCK.

         FIND FIRST bMergeMobSub EXCLUSIVE-LOCK WHERE
                    bMergeMobSub.MsSeq    EQ INT(lcParamValue)             AND
                    bMergeMobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE} NO-ERROR.

         FIND FIRST bMergeDCCLI EXCLUSIVE-LOCK WHERE
                    bMergeDCCLI.Brand   EQ Syst.Var:gcBrand  AND
                    bMergeDCCLI.DCEvent BEGINS "FTERM"       AND
                    bMergeDCCLI.MsSeq   EQ INT(lcParamValue) AND
                    bMergeDCCLI.ValidTo GT TODAY             NO-ERROR.

         FIND FIRST bMergedMsOwner EXCLUSIVE-LOCK WHERE
                    bMergedMsOwner.Brand   EQ Syst.Var:gcBrand      AND
                    bMergedMsOwner.CLI     EQ MobSub.CLI            AND
                    bMergedMsOwner.TsEnd   GT Func.Common:mMakeTS() AND
                    bMergedMsOwner.CLIType EQ MobSub.CLIType        NO-ERROR.

         IF NOT AVAIL bMergeMobSub THEN DO:
            Func.Common:mWriteMemo("MobSub",
                                   STRING(MobSub.MsSeq),
                                   MobSub.Custnum,
                                   "Merge Subscription Not Available",
                                   lcError).
            LEAVE MERGEREQUEST.
         END.

         IF NOT AVAIL bMergedMsOwner THEN DO:
            Func.Common:mWriteMemo("MobSub",
                                   STRING(MobSub.MsSeq),
                                   MobSub.Custnum,
                                   "Merged MsOwner Not Available",
                                   lcError).
            LEAVE MERGEREQUEST.
         END.

         FIND bMergeOrder NO-LOCK WHERE
              bMergeOrder.Brand   EQ Syst.Var:gcBrand     AND
              bMergeOrder.MSSeq   EQ bMergeMobSub.MsSeq   AND
              bMergeOrder.CLIType EQ bMergeMobSub.CLIType NO-ERROR.

         IF NOT AVAIL bMergeOrder THEN DO:
            FIND FIRST bMergeMsOwner NO-LOCK WHERE
                       bMergeMsOwner.CLI   EQ bMergeMobSub.CLI      AND
                       bMergeMsOwner.TsEnd GT Func.Common:mMakeTS() NO-ERROR.

            IF AVAIL bMergeMsOwner THEN DO:

               FIND FIRST bufMergeMsOwner NO-LOCK WHERE
                          bufMergeMsOwner.CLI   EQ bMergeMsOwner.CLI     AND
                          bufMergeMsOwner.TsEnd LT bMergeMsOwner.TsBegin NO-ERROR.

               IF AVAIL bufMergeMsOwner THEN DO:

                  FIND bufMergeOrder NO-LOCK WHERE
                       bufMergeOrder.Brand   EQ Syst.Var:gcBrand        AND
                       bufMergeOrder.MSSeq   EQ bMergeMobSub.MsSeq      AND
                       bufMergeOrder.CLIType EQ bufMergeMsOwner.CLIType NO-ERROR.

               END.

               IF AVAIL bufMergeOrder THEN
                  liMergeOrderId = bufMergeOrder.OrderId.

               IF NOT AVAIL bMergeOrder   AND
                  NOT AVAIL bufMergeOrder THEN DO:
                  Func.Common:mWriteMemo("MobSub",
                                         STRING(bMergeMobSub.MsSeq),
                                         bMergeMobSub.Custnum,
                                         "Merge Order Not Available",
                                         lcError).
                  LEAVE MERGEREQUEST.
               END.

            END. 
         END.
         ELSE liMergeOrderId = bMergeOrder.OrderId.

         IF llDoEvent THEN DO:
            RUN StarEventSetOldBuffer(lhMobsub).
            RUN StarEventSetOldBuffer(lhMergeMobsub).
            RUN StarEventSetOldBuffer(lhMergedMsOwner).
         END.

         ASSIGN MobSub.FixedNumber         = bMergeMobSub.FixedNumber
                bMergedMsOwner.FixedNumber = bMergeMobSub.FixedNumber           
                bMergeMobSub.FixedNumber   = ?.

         IF llDoEvent THEN DO:
            RUN StarEventMakeModifyEvent(lhMobSub).
            RUN StarEventMakeModifyEvent(lhMergedMsOwner).
            RUN StarEventMakeModifyEvent(lhMergeMobSub).
         END.

         IF AVAIL bMergeDCCLI THEN DO:

            IF llDoEvent THEN DO:
               RUN StarEventSetOldBuffer(lhMergeDCCLI).
            END.

            bMergeDCCLI.MsSeq = MobSub.MsSeq.

            IF llDoEvent THEN DO:
               RUN StarEventMakeModifyEvent(lhMergeDCCLI).
            END.

         END.

         /* Create ActionLog KeyValue field with values current  mobsub subcription id */
         /* and merged subscription order id. This would help in termination and ACC   */
         CREATE ActionLog.
         ASSIGN ActionLog.Brand        = Syst.Var:gcBrand
                ActionLog.TableName    = "MobSub"
                ActionLog.KeyValue     = STRING(MobSub.MsSeq)
                ActionLog.ActionChar   = STRING(bMergeMobSub.MsSeq) + CHR(255) + 
                                         STRING(liMergeOrderId)
                ActionLog.ActionID     = {&MERGE2P3P}
                ActionLog.CustNum      = MSRequest.Custnum
                ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS} 
                ActionLog.ActionTS     = Func.Common:mMakeTS().

         fInitialiseValues({&SUBSCRIPTION_TERM_REASON_MULTISIM},
                           fIsYoigoCLI(bMergeMobSub.CLI),
                           fIsMasmovilCLI(bMergeMobSub.CLI),
                           OUTPUT liMSISDNStat,
                           OUTPUT liSIMStat,
                           OUTPUT liQuarTime).

         llPenalty = fIsPenalty(2, bMergeMobsub.MsSeq).

         liTermReq = fTerminationRequest(bMergeMobSub.MsSeq,
                                         Func.Common:mMakeTS(),  /* when request should be handled */
                                         liMSISDNStat,
                                         liSIMStat,
                                         liQuarTime,
                                         INT(llPenalty),
                                         "",
                                         STRING({&SUBSCRIPTION_TERM_REASON_MULTISIM}),
                                         {&REQUEST_SOURCE_MERGE_STC},
                                         "",
                                         MsRequest.MsRequest,
                                         {&TERMINATION_TYPE_FULL},
                                         OUTPUT ocResult).
         IF liTermReq EQ 0 THEN
            Func.Common:mWriteMemo("MobSub",
                                   STRING(bMergeMobSub.MsSeq),
                                   bMergeMobSub.Custnum,
                                   "Merge Termination Request failed",
                                   lcError).

      END.
   END.
 
   IF bOldType.CLIType EQ "CONTM2" OR
      CLIType.CLIType EQ "CONTM2" THEN DO:
      
      RUN pCONTM2BarringReset.
      /* write possible error to a memo */
      IF RETURN-VALUE BEGINS "ERROR" THEN
         Func.Common:mWriteMemo("MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.Custnum,
                          "CONTM2 barring reset failed",
                          RETURN-VALUE).
   END.
   ELSE IF bOldType.PayType NE CLIType.PayType THEN DO:

      RUN Mm/barrengine.p(MobSub.MsSeq,
                      "Y_BPSUB=1",
                      {&REQUEST_SOURCE_STC},
                      Syst.Var:katun,               /* creator */
                      Func.Common:mSecOffSet(Func.Common:mMakeTS(),5),            /* activate */
                      "",                  /* sms */
                      OUTPUT lcError).
      liRequest = 0.
      liRequest = INTEGER(lcError) NO-ERROR.
      IF liRequest = 0 THEN
         Func.Common:mWriteMemo("MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.Custnum,
                          "Y_BPSUB barring activation failed",
                          STRING(lcError)).
   END.

   /* Send SMS once STC is done */
   RUN Mm/requestaction_sms.p(INPUT MsRequest.MsRequest,
                           INPUT MsRequest.ReqCParam2,
                           INPUT MsRequest.ReqSource).

   /* Close existing InvoiceRowCounter and update DSS consumption */
   IF DAY(ldtActDate) <> 1 AND
      bOldType.PayType = {&CLITYPE_PAYTYPE_POSTPAID} AND
      CLIType.PayType  = {&CLITYPE_PAYTYPE_POSTPAID} THEN DO:

      ASSIGN lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
             lcDataBundleCLITypes  = fCParamC("DATA_BUNDLE_BASED_CLITYPES").

      /* Only call if there is data bundle activating with iSTC */
      IF LOOKUP(MsRequest.ReqCparam2,lcDataBundleCLITypes)  = 0 AND
         LOOKUP(MsRequest.ReqCparam5,lcPostpaidDataBundles) = 0 THEN
         RUN pUpdateDSSConsumption(INPUT MsRequest.MsRequest,
                                   INPUT {&REQUEST_SOURCE_STC}).

      /* Close existing InvoiceRowCounter */
      FIND FIRST MSOwner WHERE 
                 MSOwner.MsSeq   = Mobsub.MsSeq AND
                 MSOwner.TsEnd  >= 99999999 NO-LOCK NO-ERROR.
      IF AVAIL MSOwner AND MSOwner.CLIEvent BEGINS "iS" THEN
         FOR EACH InvRowCounter EXCLUSIVE-LOCK WHERE
                  InvRowCounter.MsSeq   = MSOwner.MsSeq AND
                  InvRowCounter.InvCust = MSOwner.Custnum AND
                  InvRowCounter.ToDate >= ldtActDate:

            IF InvRowCounter.InvNum > 0 THEN DO:
               FIND FIRST Invoice NO-LOCK WHERE
                          Invoice.InvNum = InvRowCounter.InvNum NO-ERROR.
               IF NOT AVAIL Invoice OR 
                            Invoice.InvType NE 99 THEN NEXT.
            END.
            InvRowCounter.ToDate = (ldtActDate - 1).

         END. /* FOR EACH InvRowCounter WHERE */
   END. /* IF DAY(ldtActDate) <> 1 THEN DO: */
   
   RUN pUpdateCustomer.

END PROCEDURE.

PROCEDURE pUpdateCustomer:
    DEF VAR lcCategory AS CHAR NO-UNDO.

    DEF BUFFER bMobsub FOR Mobsub.    

    IF (bNewTariff.TariffType EQ {&CLITYPE_TARIFFTYPE_FIXEDONLY} OR Mobsub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) AND
      Customer.DelType NE {&INV_DEL_TYPE_NO_DELIVERY}                                                                 AND
      NOT CAN-FIND(FIRST bMobSub NO-LOCK WHERE
                         bMobsub.Brand    = Syst.Var:gcBrand        AND
                         bMobSub.CustNum  = MobSub.CustNum AND
                         bMobSub.MsSeq   <> MobSub.MsSeq   AND 
                         bMobSub.PayType  = FALSE AND
                         bMobSub.MsStatus NE {&MSSTATUS_MOBILE_NOT_ACTIVE}) THEN 
        RUN pUpdateCustomerDelType.

    IF AVAIL Order AND Order.OrderType = {&ORDER_TYPE_STC} THEN 
    DO:
        fUpdateCustomerInstAddr(Order.OrderId).
        FIND FIRST OrderCustomer WHERE OrderCustomer.Brand   = Syst.Var:gcBrand                            AND
                                       OrderCustomer.OrderID = Order.OrderId                      AND
                                       OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-LOCK NO-ERROR.
        IF AVAIL OrderCustomer AND OrderCustomer.Category > "" THEN                                        
            RUN pUpdateCustomerCategory(INPUT OrderCustomer.Category).
    END.
    ELSE
    DO:
        FIND FIRST CustCat WHERE
                   Custcat.Brand    EQ Syst.Var:gcBrand  AND
                   Custcat.category EQ Customer.Category NO-LOCK NO-ERROR.
        IF AVAIL CustCat THEN
        DO:           
            fgetCustSegment(Customer.CustIdType,
                            CustCat.SelfEmployed,
                            CustCat.Pro, 
                            Customer.OrgId,
                            OUTPUT lcCategory).
            IF Customer.Category <> lcCategory THEN 
                RUN pUpdateCustomerCategory(INPUT lcCategory).
        END.
    END.

    FIND CURRENT Customer NO-LOCK.

    RETURN "".

END PROCEDURE.

PROCEDURE pUpdateCustomerDelType:

    BUFFER Customer:FIND-CURRENT(EXCLUSIVE-LOCK, NO-WAIT).
    IF AVAIL Customer THEN 
    DO:
        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).       
        
        ASSIGN Customer.DelType = {&INV_DEL_TYPE_NO_DELIVERY}.
        
        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).
    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pUpdateCustomerCategory:
    DEFINE INPUT PARAMETER icCategory AS CHARACTER NO-UNDO.

    BUFFER Customer:FIND-CURRENT(EXCLUSIVE-LOCK, NO-WAIT).
    IF AVAIL Customer THEN 
    DO:
        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).       
        
        ASSIGN Customer.Category = icCategory.
        
        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).
    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pReRate:

   DEF INPUT PARAMETER iiMsSeq    AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiInvCust  AS INT  NO-UNDO.
   DEF INPUT PARAMETER idtActDate AS DATE NO-UNDO.

   DEF VAR ldtFrom  AS DATE NO-UNDO.
   DEF VAR ldtTo    AS DATE NO-UNDO.
   DEF VAR lcResult AS CHAR NO-UNDO.
   
   /* from the beginning of change month */
   ldtFrom = DATE(MONTH(idtActDate),1,YEAR(idtActDate)).
      
   /* to the end of it */
   ldtTo = IF MONTH(idtActDate) = 12
           THEN DATE(12,31,YEAR(idtActDate))
           ELSE DATE(MONTH(idtActDate) + 1,1,YEAR(idtActDate)) - 1.

   /* check if there is for some reason an invseq with larger scope */
   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.MsSeq     = iiMsSeq    AND    
            InvSeq.CustNum   = iiInvCust  AND
            InvSeq.FromDate <= idtActDate AND
            InvSeq.ToDate   >= idtActDate AND
            InvSeq.Billed    = FALSE:
      ldtFrom = MIN(ldtFrom,InvSeq.FromDate).       
   END.   
      
   IF ldtTo > ldtFrom THEN DO:
         
      fReqLog("Rerate " + STRING(ldtFrom,"99.99.99") + "-" +
                          STRING(ldtTo,"99.99.99")).

      fRerateRequest(iiInvCust,
                     iiMsSeq,
                     ldtFrom,
                     ldtTo,
                     TRUE,    /* wait for other possible subrequests */
                     FALSE, 
                     0,       /* activate now */
                     "",      /* creator */
                     {&REQUEST_SOURCE_STC},     /* source */
                     MsRequest.MsRequest,
                     0,       /* not mandatory */
                     OUTPUT lcResult).
   END.
 
END PROCEDURE.

PROCEDURE pCloseContracts:

   DEF INPUT PARAMETER iiMainRequest AS INT  NO-UNDO.
   DEF INPUT PARAMETER icNewType     AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icBaseBundle  AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiMsSeq       AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiCustNum     AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaActDate    AS DATE NO-UNDO.
   DEF INPUT PARAMETER idEndStamp    AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icReqSource   AS CHAR NO-UNDO.

   DEF BUFFER bActRequest  FOR MsRequest.
   DEF BUFFER bOrigRequest FOR MsRequest.

   DEF VAR lcContractList AS CHAR NO-UNDO.
   DEF VAR lcContIDList   AS CHAR NO-UNDO.
   DEF VAR liCount        AS INT  NO-UNDO.
   DEF VAR liTerminate    AS INT  NO-UNDO.
   DEF VAR lcError        AS CHAR NO-UNDO.
   DEF VAR llCreated      AS LOG  NO-UNDO INIT FALSE.
   DEF VAR liRequest      AS INT  NO-UNDO. 
   DEF VAR ldeActStamp    AS DEC  NO-UNDO.
   DEF VAR lcContract     AS CHAR NO-UNDO.
   DEF VAR liContractID   AS INT  NO-UNDO.
   DEF VAR liPeriod       AS INT  NO-UNDO.
   DEF VAR llCloseRVTermFee AS LOG NO-UNDO INIT TRUE.

   DEF VAR liBonoTerminate           AS INT     NO-UNDO INIT 0.
   DEF VAR lcAllowedBONOSTCContracts AS CHAR    NO-UNDO.
   DEF VAR lcOnlyVoiceContracts      AS CHAR    NO-UNDO.
   DEF VAR lcBONOContracts           AS CHAR    NO-UNDO.
   DEF VAR lcAllVoIPNativeBundles    AS CHAR    NO-UNDO.
   DEF VAR llCreateFees                         AS LOG NO-UNDO.
   DEF VAR llIsSTCBetweenConvergent             AS LOG NO-UNDO.
   DEF VAR llIsSTCBetweenFixedOnlyAndConvergent AS LOG NO-UNDO.

   EMPTY TEMP-TABLE ttContract.

   FIND FIRST bOrigRequest WHERE bOrigRequest.MsRequest = iiMainRequest NO-LOCK NO-ERROR.

   /* end old bundles to the end of previous month */
   IF DAY(idaActDate) = 1 AND llOldPayType = FALSE THEN
      idEndStamp = Func.Common:mMake2DT(idaActDate - 1,86399).

   ASSIGN ldeActStamp = Func.Common:mSecOffSet(idEndStamp,1)
          lcAllowedBONOSTCContracts = fCParamC("ALLOWED_BONO_STC_CONTRACTS")
          lcBONOContracts           = fCParamC("BONO_CONTRACTS")
          lcAllVoIPNativeBundles    = fCParamC("NATIVE_VOIP_BASE_BUNDLES").

   IF icNewType = "CONTF" THEN
      lcOnlyVoiceContracts = fCParamC("ONLY_VOICE_CONTRACTS").

   IF NOT (CLIType.PayType  = {&CLITYPE_PAYTYPE_PREPAID} AND bOldType.PayType = {&CLITYPE_PAYTYPE_POSTPAID}) THEN
       llCloseRVTermFee = FALSE.
   ELSE 
       liPeriod = YEAR(idaActDate - 1) * 100 + MONTH(idaActDate - 1).
   
   IF ((CLIType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY}  AND bOldType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT}) OR 
       (CLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT} AND bOldType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY})) THEN
       ASSIGN llIsSTCBetweenFixedOnlyAndConvergent = TRUE.
   ELSE IF CLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT} AND bOldType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN
       ASSIGN llIsSTCBetweenConvergent = TRUE.

   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq   = iiMsSeq  AND
            DCCLI.ValidTo >= idaActDate,
      FIRST DayCampaign NO-LOCK WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCevent:

      /* pending termination request */
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.MsSeq = iiMsSeq AND
                        MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ReqCParam3 = DCCLI.DCEvent AND
                        LOOKUP(STRING(MsRequest.ReqStatus),
                                {&REQ_INACTIVE_STATUSES}) = 0 AND
                        MsRequest.ActStamp <= ldeActStamp AND
                        (IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT}
                         THEN MsRequest.ReqIParam3 = DCCLI.PerContractId
                         ELSE TRUE)) THEN NEXT.

      lcContractList = lcContractList + 
                       (IF lcContractList > "" THEN "," ELSE "") +
                       DCCLI.DCEvent.
                              
      lcContIDList   = lcContIDList + (IF lcContIDList > "" THEN "," ELSE "") +
                       (IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT}
                        THEN STRING(DCCLI.PerContractID)
                        ELSE STRING(0)).

      /* Reset the flag if leasing installment contract is being terminated */
      IF DCCLI.DCEvent BEGINS "PAYTERM" AND
         DCCLI.Amount > 0 THEN llCloseRVTermFee = FALSE.

   END.

   lcContractList = lcContractList + 
                    (IF lcContractList > "" THEN "," ELSE "") + 
                    fGetActiveBundle(iiMsSeq,ldeActStamp).
          
   /* this rule is 'allowed' type, so result should be 1 (if no rule for
      contract id found then allowed) */
   DO liCount = 1 TO NUM-ENTRIES(lcContractList):

      ASSIGN lcContract   = ENTRY(liCount,lcContractList).

      FIND FIRST DayCampaign NO-LOCK WHERE
                 DayCampaign.Brand = Syst.Var:gcBrand AND
                 DayCampaign.DCEvent = lcContract NO-ERROR.

      IF AVAIL DayCampaign AND
               DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} THEN 
         liContractID = INT(ENTRY(liCount,lcContIDList)). 

      IF (lcContract EQ "BONO_VOIP" AND
         (LOOKUP(icNewType,lcAllVoIPNativeBundles) > 0 OR 
          LOOKUP(icBaseBundle,lcAllVoIPNativeBundles) > 0))
         OR
         (fMatrixAnalyse(Syst.Var:gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                         lcContract + ";" + icNewType,
                        OUTPUT lcReqChar) NE 1 AND
          ENTRY(1,lcReqChar,";") NE "?") 
         OR
         /* Since, convergent base bundles CONTDSL/CONTFH50/CONTFH300 are reused in fixed only convergent also with different prices.
            Above matrix condition will fail and convergent base bundles are not terminated for prices to change. So, below is introduced. */
         ((llIsSTCBetweenFixedOnlyAndConvergent OR llIsSTCBetweenConvergent) AND LOOKUP(lcContract,{&YOIGO_CONVERGENT_BASE_BUNDLES_LIST}) > 0) 
         OR
         (LOOKUP(lcContract,lcBonoContracts) > 0 AND LOOKUP(lcContract,lcAllowedBonoSTCContracts) = 0) THEN 
      DO:
         /* YDR-2038 (stc/btc to prepaid)
            ReqIParam5
            (0=no extend_term_contract
             1=extend_term_contract
             2=exclude_term_penalty)
          */
         IF AVAILABLE(bOrigRequest) AND bOrigRequest.ReqIParam5 EQ 2 AND
            CAN-FIND(FIRST DayCampaign NO-LOCK WHERE DayCampaign.Brand   EQ Syst.Var:gcBrand             AND 
                                                     DayCampaign.DCEvent EQ lcContract          AND 
                                                     DayCampaign.DCType  EQ {&DCTYPE_DISCOUNT}) THEN 
             llCreateFees = FALSE.
         ELSE 
             llCreateFees = TRUE. 

         /* terminate periodical contract */
         liTerminate = fPCActionRequest(iiMsSeq,
                                        lcContract,
                                        "term",
                                        idEndStamp,
                                        llCreateFees,   /* create fee */
                                        icReqSource,
                                        "",
                                        iiMainRequest,
                                        TRUE,   /* mandatory subreq. */
                                        (IF lcContract EQ "PMDUB" THEN "PMDUBDeActSTC" ELSE ""), /* SMS for PMDUB STC Deactivation */
                                        0,
                                        (IF AVAIL DayCampaign AND DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} THEN liContractID ELSE 0),
                                        "",
                                        OUTPUT lcError).
         IF liTerminate = 0 THEN
            Func.Common:mWriteMemo("MobSub",
                       STRING(iiMsSeq),
                       MobSub.CustNum,
                       "Subscription type change",
                       "Per.contract termination request creation failed; " +
                          lcError).                
         ELSE DO:
            IF LOOKUP(lcContract,lcBonoContracts) > 0 THEN
               liBonoTerminate = liTerminate.
            llCreated = TRUE.
         END.
      END. /* IF fMatrixAnalyse(Syst.Var:gcBrand, */
   END. /* DO liCount = 1 TO NUM-ENTRIES(lcContractList): */

   IF LOOKUP("PMDUB", lcContractList) > 0 AND
     /* PMDUB HSDPA need to be resent (SERVICECLASS=0081/0084), SER-1345 */
      liBonoTerminate = 0 THEN DO:

       liRequest = fServiceRequest(MobSub.MsSeq,
                       "HSDPA",
                       1, /* on */
                       "",
                       Func.Common:mMakeTS(),
                       "",
                       FALSE, /* fees */
                       FALSE, /* sms */
                       "",
                       {&REQUEST_SOURCE_STC},
                       iiMainRequest, /* father request */
                       TRUE, /* mandatory for father request */
                       OUTPUT lcError).

      IF liRequest = 0 THEN
         Func.Common:mWriteMemo("MobSub",
                    STRING(iiMsSeq),
                    MobSub.CustNum,
                    "Subscription type change",
                    "HSDPA activation request creation failed; " +
                    lcError).
      ELSE llCreated = TRUE.
   END.

   /* Handle BB, BONO_VOIP - Only for CONT,CONT4,CONT5,CONTF10,CONTF20 */
   IF LOOKUP(icNewType,"CONT,CONT4,CONT5") > 0 OR
      (icBaseBundle > "" AND LOOKUP(icBaseBundle,lcOnlyVoiceContracts) > 0)
   THEN DO:

      /* Suspend the BB service */
      IF (INDEX(lcContractList,"MDUB") = 0 AND 
          INDEX(lcContractList,"DATA") = 0) OR
         liBonoTerminate > 0 THEN DO:

         RUN pChangedBBStatus(INPUT 2,
                              INPUT ldeActStamp,
                              INPUT {&REQUEST_SOURCE_STC},
                              BUFFER MsRequest,
                              BUFFER MobSub).

         /* Deactivate BONO_VOIP */
         IF LOOKUP("BONO_VOIP",lcContractList) > 0 THEN DO:
            CREATE ttContract.
                   ttContract.DCEvent = "BONO_VOIP".
         END.
      END. /* IF INDEX(lcContractList,"MDUB") = 0 THEN DO: */

      /* Modify BB profile */
      ElSE IF (LOOKUP("MDUB2",lcContractList) > 0 OR
         LOOKUP("MDUB3",lcContractList) > 0 OR
         LOOKUP("MDUB4",lcContractList) > 0) THEN
         RUN pChangedBBStatus(INPUT 1,
                              INPUT ldeActStamp,
                              INPUT {&REQUEST_SOURCE_STC},
                              BUFFER MsRequest,
                              BUFFER MobSub).
   END. /* IF LOOKUP(icNewType,"CONT,CONT4,CONT5,CONTM2") > 0 */

   FOR EACH ttContract:
      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand   = Syst.Var:gcBrand AND
                 DayCampaign.DCEvent = ttContract.DCEvent AND
                 DayCampaign.ValidTo >= Today NO-LOCK NO-ERROR.
      IF NOT AVAIL DayCampaign THEN DO:
         Func.Common:mWriteMemo("MobSub",
                          STRING(Mobsub.MsSeq),
                          Mobsub.CustNum,
                          "Periodical Contract",
                          ttContract.DCEvent +
                          ": Periodical contract information is missing!").
         DELETE ttContract.
         NEXT.
      END. /* IF NOT AVAIL DayCampaign THEN DO: */

      liRequest = fPCActionRequest(MobSub.MsSeq,
                       ttContract.DCEvent,
                       "term",
                       idEndStamp,
                       TRUE,             /* create fees */
                       icReqSource,
                       "",
                       iiMainRequest, /* Father Request */
                       FALSE,
                       "",
                       0,
                       0,
                       "",
                       OUTPUT lcError).
      IF liRequest = 0 THEN
         /* Write memo */
         Func.Common:mWriteMemo("MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.CustNum,
                          "Periodical Contract",
                          ttContract.DCEvent +
                          ": Periodical contract is not closed: " + lcError).
   END. /* FOR EACH ttContract: */

   EMPTY TEMP-TABLE ttContract NO-ERROR.

   /* Deactivate VoIPVideo if new tariff is not native VoIP */
   IF LOOKUP("BONO_VOIP",lcContractList) = 0 AND
      AVAIL bOrigRequest AND
      (LOOKUP(bOrigRequest.ReqCparam1,lcAllVoIPNativeBundles) > 0 OR
       LOOKUP(bOldTariff.CLIType,lcAllVoIPNativeBundles) > 0) AND
      LOOKUP(icNewType,lcAllVoIPNativeBundles) = 0 AND
      LOOKUP(icBaseBundle,lcAllVoIPNativeBundles) = 0 AND
      NOT fIsDSSActive(MobSub.CustNum,bOrigRequest.ActStamp) THEN DO:
      liRequest = fServiceRequest(MobSub.MsSeq,
                                  "VOIPVIDEO",
                                  0,
                                  "",
                                  bOrigRequest.ActStamp,
                                  "",
                                  FALSE, /* fees */
                                  FALSE, /* sms */
                                  "",
                                  icReqSource,
                                  bOrigRequest.MsRequest, /* father request */
                                  FALSE,
                                  OUTPUT lcError).
      IF liRequest = 0 THEN
         Func.Common:mWriteMemo("MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.CustNum,
                          "VOIPVIDEO",
                          "VOIPVIDEO deactivation request failed; " +
                          lcError).
   END.

   /* Close Residual Amount Single Fee */
   IF llCloseRVTermFee THEN
      FOR EACH SingleFee USE-INDEX Custnum WHERE
               SingleFee.Brand       = Syst.Var:gcBrand AND
               SingleFee.Custnum     = MobSub.CustNum AND
               SingleFee.HostTable   = "Mobsub" AND
               SingleFee.KeyValue    = STRING(MobSub.MsSeq) AND
               SingleFee.BillPeriod  > liPeriod AND
               SingleFee.CalcObj     = "RVTERM" EXCLUSIVE-LOCK:
         
         IF SingleFee.Invnum > 0 AND
            CAN-FIND(FIRST Invoice NO-LOCK WHERE
                           Invoice.Invnum = SingleFee.Invnum AND
                           Invoice.InvType NE 99) THEN NEXT.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).
            ASSIGN SingleFee.BillPeriod  = liPeriod
                   SingleFee.Concerns[1] = YEAR(idaActDate - 1) * 10000 + 
                                           MONTH(idaActDate - 1) * 100  +
                                           DAY(idaActDate - 1).
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSingleFee).
      END.

   IF llCreated THEN RETURN "SubRequests created".
   ELSE RETURN "".
   
END PROCEDURE.

PROCEDURE pNetworkAction:

   DEF VAR liOffSet AS INT  NO-UNDO.
   DEF VAR liPrepaidBalanceInCents AS INT NO-UNDO. 
   
   liOffSet = 0.
   
   /* unbarring is done before stc-solog; find barrings that are done to 
      old clitype during activation and unbarr them */
   FOR EACH RequestAction NO-LOCK WHERE
            RequestAction.Brand      = Syst.Var:gcBrand AND
            RequestAction.CLIType    = MsRequest.ReqCParam1  AND
            RequestAction.ReqType    = 13                    AND
            RequestAction.ValidTo   >= MobSub.ActivationDate AND
            RequestAction.ValidFrom <= MobSub.ActivationDate AND
            RequestAction.ActionType = "CTServPac" AND 
            RequestAction.Action     = 1:

      IF fGetBarringStatus(RequestAction.ActionKey,
                           MobSub.MsSeq) EQ {&BARR_STATUS_ACTIVE} THEN
         RUN Mm/barrengine.p(MobSub.MsSeq,
                         RequestAction.ActionKey + "=0",
                         {&REQUEST_SOURCE_STC}, /* source  */
                         "",                  /* creator */
                         MsRequest.ActStamp,  /* activate */
                         "",                  /* sms */
                         OUTPUT lcError).

      /* if unbarr fails, stc can still go on? */

      /* delay for actual stc-solog, so that unbarring is done by then */
      liOffSet = 300.
   END.

   /* YOT-627 */
   IF bOldType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID} AND
      CLIType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID} THEN DO:

      RUN Gwy/balancequery.p(MobSub.CLI).
      
      FIND CURRENT MsRequest EXCLUSIVE-LOCK.

      liPrepaidBalanceInCents = INT(RETURN-VALUE) NO-ERROR.
      
      IF RETURN-VALUE BEGINS "ERROR:" OR ERROR-STATUS:ERROR THEN DO:
         CREATE ErrorLog.
         ASSIGN ErrorLog.Brand = Syst.Var:gcBrand
                ErrorLog.TableName = "MsRequest"
                ErrorLog.KeyValue = STRING(MsRequest.MsRequest)
                ErrorLog.ActionID = "STCBalanceQuery"
                ErrorLog.ActionTS = Func.Common:mMakeTS()
                ErrorLog.ErrorMsg = (IF RETURN-VALUE BEGINS "ERROR:" THEN
                                    ENTRY(2,RETURN-VALUE,":") ELSE RETURN-VALUE)
                MsRequest.ReqIParam3 = 0.
      END.
      ELSE IF liPrepaidBalanceInCents > 0 THEN DO:
         MsRequest.ReqIParam3 = liPrepaidBalanceInCents.
      END.
   END.
   
   /* stc solog needed only when service class changes */ 
   IF bOldType.ServiceClass ne CliType.ServiceClass THEN DO:
   
      IF liOffSet > 0 THEN DO:
         FIND CURRENT MsRequest EXCLUSIVE-LOCK.
         MsRequest.ReqIParam4 = liOffSet.
      END.
      
      RUN Gwy/createsolog.p(MSRequest.MSRequest).
   
      RETURN "SubRequest created".
   END.

   RETURN "".

END PROCEDURE.

PROCEDURE pUpdateDSSAccount:

   DEF INPUT PARAMETER iiMainRequest AS INT  NO-UNDO.
   DEF INPUT PARAMETER ideActStamp   AS DEC  NO-UNDO.
   DEF INPUT PARAMETER idActDate     AS DATE NO-UNDO.
   DEF INPUT PARAMETER icUserCode    AS CHAR NO-UNDO.

   DEF VAR liDSSMsSeq                AS INT  NO-UNDO.
   DEF VAR ldeDSSLimit               AS DEC  NO-UNDO.
   DEF VAR ldeEndStamp               AS DEC  NO-UNDO.
   DEF VAR ldEndDate                 AS DATE NO-UNDO.
   DEF VAR liRequest                 AS INT  NO-UNDO.
   DEF VAR lcError                   AS CHAR NO-UNDO.
   DEF VAR llDSSTransferred          AS LOG  NO-UNDO.
   DEF VAR lcBundleId                AS CHAR NO-UNDO.
   DEF VAR llOldSubTypeRemove        AS LOG  NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType     AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS4SubsType     AS CHAR NO-UNDO.  
   DEF VAR ldeDataBundleLimit        AS DEC  NO-UNDO.
   DEF VAR ldeLastDayEndStamp        AS DEC  NO-UNDO.
   DEF VAR lcDSSBunldeId             AS CHAR NO-UNDO. 
   DEF VAR lcDSSId                   AS CHAR NO-UNDO. 
   DEF VAR llgMatrixAvailable        AS LOG  NO-UNDO INITIAL FALSE. 
   DEF VAR llgRemoveELMatrix         AS LOG  NO-UNDO INITIAL FALSE.

   DEF BUFFER bELMobSub     FOR MobSub.   
   DEF BUFFER bMobSub       FOR MobSub.
   DEF BUFFER bTerMsRequest FOR MsRequest.

   /* end old bundles to the end of previous tariff period */
   ASSIGN ldEndDate          = idActDate - 1
          ldeEndStamp        = Func.Common:mMake2DT(ldEndDate,86399)
          ldeLastDayEndStamp = Func.Common:mMake2DT(Func.Common:mLastDayOfMonth(ldEndDate),86399).

   ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").
          lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE").

   /* Check whether DSS bundle is active or not for this customer */
   IF NOT fGetDSSMsSeqLimit(INPUT MobSub.CustNum,
                            INPUT ideActStamp,
                            OUTPUT liDSSMsSeq,
                            OUTPUT ldeDSSLimit,
                            OUTPUT lcBundleId) THEN DO:
      fUpdateDSSAccount(MobSub.MsSeq,
                        {&REQUEST_SOURCE_STC},
                        iiMainRequest,
                        ideActStamp,
                        "CREATE").
      RETURN.
   END.

   /* If DSS/DSS2/DSS4 is active */
   CASE bOldType.PayType:
      
      WHEN {&CLITYPE_PAYTYPE_POSTPAID} THEN DO:
         
         IF CLIType.PayType                               EQ {&CLITYPE_PAYTYPE_POSTPAID} AND
           (LOOKUP(CLIType.CLIType,lcAllowedDSS4SubsType) GT 0 OR
            LOOKUP(CLIType.CLIType,lcAllowedDSS2SubsType) GT 0)                          THEN DO:
            
            IF LOOKUP(lcBundleId,SUBSTITUTE("&1,&2,&3",{&DSS},{&DSS2},{&DSS4})) EQ 0 THEN
               RETURN.

            fUpdateDSSAccount(MobSub.MsSeq,
                              {&REQUEST_SOURCE_STC},
                              iiMainRequest,
                              ideActStamp,
                              "CREATE").
         END. 
   
         CASE lcBundleId:
            WHEN {&DSS4} THEN DO:
               IF (LOOKUP(bOldType.CLIType,lcAllowedDSS4SubsType) GT 0  AND
                   LOOKUP(CLIType.CLIType,lcAllowedDSS4SubsType)  EQ 0) THEN
               llOldSubTypeRemove = TRUE.
            END.
            WHEN {&DSS2} THEN DO:
               IF (LOOKUP(bOldType.CLIType,lcAllowedDSS2SubsType) GT 0  AND
                   LOOKUP(CLIType.CLIType,lcAllowedDSS2SubsType)  EQ 0) THEN
               llOldSubTypeRemove = TRUE.
            END.
            WHEN {&DSS} THEN DO:
               IF bOldType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID} AND 
                  CLIType.PayType  EQ {&CLITYPE_PAYTYPE_PREPAID}  THEN
               llOldSubTypeRemove = TRUE.
            END.
         END CASE.

         /* Postpaid to Prepaid - remove subs. from DSS group      */
         /* If this subs. is directly linked to DSS then either    */
         /* transfer DSS to OLD postpaid subs. or delete DSS group */
         IF llOldSubTypeRemove THEN DO:

            IF LOOKUP(lcBundleId,SUBSTITUTE("&1,&2",{&DSS2},{&DSS4})) GT 0                 AND 
               (fCLITypeIsMainLine(MobSub.CLIType) OR fCLITypeIsExtraLine(MobSub.CLIType)) THEN 
            llgRemoveELMatrix = TRUE.   

            fUpdateDSSAccount(MobSub.MsSeq,
                              {&REQUEST_SOURCE_STC},
                              iiMainRequest,
                              ideActStamp,
                              "DELETE").

         END. 
      END. 
      WHEN {&CLITYPE_PAYTYPE_PREPAID} THEN DO:
         IF CLIType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID} THEN
            fUpdateDSSAccount(MobSub.MsSeq,
                              {&REQUEST_SOURCE_STC},
                              iiMainRequest,
                              ideActStamp,
                              "CREATE").
         ELSE RETURN.
      END. 
      OTHERWISE RETURN.
   END CASE.

END PROCEDURE. /* PROCEDURE pUpdateDSSAccount: */

PROCEDURE pActivateTARJ5PromotionalPrice:

   DEF OUTPUT PARAM ocError AS CHAR NO-UNDO. 

   DEF VAR lcCurrentBundle    AS CHAR NO-UNDO.
   DEF VAR liTempServiceClass AS INT NO-UNDO. 
   DEF VAR liServiceClass AS INT NO-UNDO. 

   FOR EACH MSOwner NO-LOCK WHERE
            MSOwner.MSSeq = MsRequest.MsSeq USE-INDEX MsSeq:
      
      IF MSOwner.CLIType BEGINS "CONT" THEN LEAVE.
      IF MSOwner.CLIType EQ "TARJ5" THEN RETURN.
   END.

   lcCurrentBundle = fGetCurrentBundle(MsRequest.MsSeq).

   IF LOOKUP("PMDUB",lcCurrentBundle) > 0 THEN ASSIGN
      liServiceClass = {&SC_TARJ5_NORMAL_BONO}
      liTempServiceClass = {&SC_TARJ5_PROMOTIONAL_BONO}.
   ELSE ASSIGN
      liServiceClass = {&SC_TARJ5_NORMAL}
      liTempServiceClass = {&SC_TARJ5_PROMOTIONAL}.

   RUN Gwy/air_update_serviceclass.p(MsRequest.CLI,
                                 liServiceClass,
                                 liTempServiceClass,
                                 TODAY + 31,
                                 OUTPUT ocError).

END PROCEDURE. 

PROCEDURE pMultiSimSTC:

   DEF INPUT PARAMETER idtActDate AS DATE NO-UNDO.
 
   DEF VAR liQuarTime          AS INT  NO-UNDO.
   DEF VAR liSimStat           AS INT  NO-UNDO.
   DEF VAR liMSISDNStat        AS INT  NO-UNDO.
   DEF VAR liRequest           AS INT  NO-UNDO.
   DEF VAR ldaSTCCreateDate    AS DATE NO-UNDO.
   DEF VAR liSTCCreateTime     AS INT  NO-UNDO.
   DEF VAR ldaSecSIMTermDate   AS DATE NO-UNDO.
   DEF VAR ldeSecSIMTermStamp  AS DEC  NO-UNDO.

   DEF BUFFER lbMobSub    FOR Mobsub.

   IF NOT AVAIL Mobsub THEN RETURN.

   FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
              lbMobSub.Brand  = Syst.Var:gcBrand AND
              lbMobSub.MultiSimID = MobSub.MultiSimID AND
              lbMobSub.MultiSimType NE MobSub.MultiSimType AND
              lbMobSub.Custnum = MobSub.Custnum NO-ERROR.
   IF NOT AVAIL lbMobSub THEN DO:
      /* Terminate MultiSIM Link */
      FIND CURRENT Mobsub EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN MobSub.MultiSimID = 0
             MobSub.MultiSimType = 0.
      FIND CURRENT Mobsub NO-LOCK NO-ERROR.

      RETURN.
   END.

   CREATE ActionLog.
   ASSIGN
      ActionLog.ActionTS     = Func.Common:mMakeTS()
      ActionLog.Brand        = Syst.Var:gcBrand
      ActionLog.TableName    = "Customer"
      ActionLog.KeyValue     = STRING(MobSub.Custnum)
      ActionLog.UserCode     = Syst.Var:katun
      ActionLog.ActionID     = "MultiSIMTermination"
      ActionLog.ActionPeriod = YEAR(idtActDate - 1) * 100 +
                               MONTH(idtActDate - 1)
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
      ActionLog.ActionChar   = SUBST("STC subscription: &1, " + "
                                      MultiSIMID: &2, MultiSimType &3",
                               MobSub.MsSeq, MobSub.MultiSimID,
                               MobSub.MultiSIMType).

   /* If there is no ongoing STC/termination request for secondary line */
   IF Mobsub.MultiSimType EQ {&MULTISIMTYPE_PRIMARY} AND
      NOT CAN-FIND (FIRST MsRequest WHERE
          MsRequest.MsSeq   = lbMobSub.Msseq AND
          MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
          LOOKUP(STRING(MsRequest.ReqStatus),
                 {&REQ_INACTIVE_STATUSES}) = 0) AND
      NOT CAN-FIND (FIRST MsRequest WHERE
          MsRequest.MsSeq   = lbMobSub.Msseq AND
          MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
          LOOKUP(STRING(MsRequest.ReqStatus),
                 {&REQ_INACTIVE_STATUSES}) = 0) AND
      NOT Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT lbMobSub.CLI) THEN DO:

      fTermAdditionalSim(lbMobSub.Msseq,
                         lbMobSub.CLI,
                         lbMobSub.CustNum,
                         {&SUBSCRIPTION_TERM_REASON_MULTISIM},
                         idtActDate,
                         {&REQUEST_SOURCE_STC},
                         iiMsRequest,
                         OUTPUT lcError).

      IF lcError = "" THEN 
         RUN pSendSMS(
          MobSub.MsSeq,
          MsRequest.MsRequest,
          "MultiSIMPrimarySTC",
          {&SMSTYPE_STC},
          "22622",
          "").
   END. /* IF Mobsub.MultiSimType EQ {&MULTISIMTYPE_PRIMARY} THEN DO: */

   /* Terminate MultiSIM Link */
   FIND CURRENT Mobsub EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN MobSub.MultiSimID = 0
          MobSub.MultiSimType = 0.
   FIND CURRENT Mobsub NO-LOCK NO-ERROR.

END PROCEDURE. 

PROCEDURE pCONTM2BarringReset:

   DEF VAR llOngoing AS LOG NO-UNDO.
   DEF VAR lrBarring AS ROWID NO-UNDO.
   DEF VAR liLoop AS INT NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO. 
   DEF VAR lcBarring AS CHAR NO-UNDO. 
   DEF VAR llHasActiveBarringComponent AS LOG NO-UNDO. 
   DEF VAR liRequest AS INT NO-UNDO. 

   IF NOT AVAIL Mobsub OR
      NOT AVAIL MsRequest OR
      NOT AVAIL CLiType OR 
      NOT AVAIL bOldType THEN RETURN "OK".

   IF NOT (bOldType.CLIType EQ "CONTM2" OR
            CLIType.CLIType EQ "CONTM2") THEN RETURN "OK".
   
   llOngoing = fCheckBarrStatus(MobSub.MsSeq,
                                OUTPUT lcBarring,
                                OUTPUT lrBarring).
      
   IF llOngoing THEN RETURN "ERROR:Ongoing barring request".
   
   BARR_CONF_LOOP:
   FOR EACH Barring NO-LOCK WHERE
            Barring.MsSeq EQ MobSub.MsSeq
      USE-INDEX MsSeq BREAK BY Barring.BarringCode:

      IF FIRST-OF(BarringCode) AND
         Barring.BarringStatus EQ {&BARR_STATUS_ACTIVE} THEN DO:
         IF CAN-FIND(FIRST BarringConf NO-LOCK WHERE
                           BarringConf.BarringCode = Barring.BarringCode AND
                           BarringConf.NWComponent EQ "BARRING") THEN DO:
             llHasActiveBarringComponent = TRUE.
             LEAVE BARR_CONF_LOOP.
         END.
      END.
   END.

   IF llHasActiveBarringComponent THEN DO:

      RUN Mm/barrengine.p(MobSub.MsSeq,
                      "#REFRESH",
                      {&REQUEST_SOURCE_STC},
                      Syst.Var:katun,               /* creator */
                      Func.Common:mSecOffSet(Func.Common:mMakeTS(),2),            /* activate */
                      "",                  /* sms */
                      OUTPUT lcError).
      liRequest = 0.
      liRequest = INTEGER(lcError) NO-ERROR.
      IF liRequest = 0 THEN RETURN "ERROR:Barring refresh failed:" + STRING(lcError).
   END.
   ELSE DO:
      liRequest = fServiceRequest(MobSub.MsSeq,
                       "BARRING",
                       1, /* on */
                       (IF CLIType.CLIType EQ "CONTM2"
                        THEN "0110000"
                        ELSE "0000000"),
                       Func.Common:mSecOffSet(Func.Common:mMakeTS(),5),
                       "",
                       FALSE, /* fees */
                       FALSE, /* sms */
                       "",
                       {&REQUEST_SOURCE_STC},
                       MsRequest.msrequest, /* father request */
                       FALSE, /* mandatory for father request */
                       OUTPUT lcError).

      IF liRequest = 0 THEN
         RETURN "ERROR:Barring service request failed:" + STRING(lcError).
      
      IF bOldType.PayType NE CLIType.PayType THEN DO:
         RUN Mm/barrengine.p(MobSub.MsSeq,
                         "Y_BPSUB=1",
                         {&REQUEST_SOURCE_STC},
                         Syst.Var:katun,               /* creator */
                         Func.Common:mSecOffSet(Func.Common:mMakeTS(),6),            /* activate */
                         "",                  /* sms */
                         OUTPUT lcError).
         liRequest = 0.
         liRequest = INTEGER(lcError) NO-ERROR.
         IF liRequest = 0 THEN RETURN "ERROR:Y_BPSUB activation failed:" + STRING(lcError).
      END.
   END.
      
   RETURN "OK".
END PROCEDURE. 

