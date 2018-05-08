/* ----------------------------------------------------------------------
  MODULE .......: ffitemdump.p
  TASK .........: Fixed fee item dump to Track
  APPLICATION ..: TMS
  AUTHOR .......: Chanchal Sharma
  CREATED ......: 31.03.2017
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

DISABLE TRIGGERS FOR DUMP OF FixedFee.
DISABLE TRIGGERS FOR DUMP OF FFItem.

{Syst/commali.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/finvnum.i}
{Mm/fbundle.i}
{Syst/dumpfile_run.i}

DEF INPUT PARAMETER icDumpID       AS INT  NO-UNDO.
DEF INPUT PARAMETER icFile         AS CHAR NO-UNDO.
DEF INPUT PARAMETER icDumpMode     AS CHAR NO-UNDO.
DEF INPUT PARAMETER idLastDump     AS DEC  NO-UNDO.
DEF INPUT PARAMETER icEventSource  AS CHAR NO-UNDO.
DEF INPUT PARAMETER icEventFields  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric          AS CHAR NO-UNDO.
DEF VAR lcDelimiter        AS CHAR NO-UNDO.
DEF VAR ldaFromDate        AS DATE NO-UNDO.
DEF VAR ldaToDate          AS DATE NO-UNDO.
DEF VAR ldaPrevFromDate    AS DATE NO-UNDO.
DEF VAR ldPeriodFrom       AS DEC  NO-UNDO.
DEF VAR ldPeriodFromSTC    AS DEC  NO-UNDO.
DEF VAR ldPeriodTo         AS DEC  NO-UNDO.
DEF VAR lcIPLContracts     AS CHAR NO-UNDO.
DEF VAR lcBONOContracts    AS CHAR NO-UNDO.
DEF VAR ldeStart           AS DEC NO-UNDO. 
DEF VAR ldeEnd             AS DEC NO-UNDO. 
DEF VAR liPeriod           AS INT  NO-UNDO.
DEF VAR liPrevPeriod       AS INT  NO-UNDO.
DEF VAR liDayOfMonth       AS INT  NO-UNDO.
DEF VAR liKey              AS INTEGER NO-UNDO.

DEF VAR lcAllowedDSS2SubsType         AS CHAR NO-UNDO.
DEF VAR lcExcludeBundles              AS CHAR NO-UNDO.
DEF VAR lcFirstMonthUsageBasedBundles AS CHAR  NO-UNDO.

DEF TEMP-TABLE ttSubscription NO-UNDO
    FIELD MsSeq        AS INT
    FIELD ServiceLimit AS CHAR   
    INDEX MsSeq MsSeq.      

DEF TEMP-TABLE ttData
    FIELD ffitemnum  LIKE FFItem.FFItemNum
    FIELD ffnum      LIKE FixedFee.FFNum
    FIELD custnum    LIKE FFItem.CustNum
    FIELD billper    LIKE FFItem.BillPeriod
    FIELD billcode   LIKE FFItem.BillCode
    FIELD calcmethod AS CHAR
    FIELD msseq      LIKE FixedFee.KeyValue
    FIELD cli        LIKE FixedFee.Cli
    FIELD billed     LIKE FFItem.Billed
    FIELD feeamt     LIKE FFItem.Amt
    FIELD createdate LIKE FixedFee.BegDate
    INDEX idx1 IS PRIMARY ffitemnum.

lcNumeric = SESSION:NUMERIC-FORMAT.
 
FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   ASSIGN lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
   
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN 
      lcDelimiter = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

/* STREAM fixfeeitem is for ffitem file */
DEF STREAM fixfeeitem.
OUTPUT STREAM fixfeeitem TO VALUE(icFile).

PUT STREAM fixfeeitem UNFORMATTED
    "FIXEDFEEITEMNUM" lcDelimiter "FIXEDFEENUM" lcDelimiter "CUSTNUM" lcDelimiter "BILLPERIOD" lcDelimiter "BILLCODE" lcDelimiter 
     "CALCMETHOD" lcDelimiter "MSSEQ" lcDelimiter "MSISDN" lcDelimiter "BILLED" lcDelimiter "FEEAMT" lcDelimiter "CREATEDATE" SKIP.

{Mm/dss_bundle_first_month_fee.i}

ASSIGN ldaFromDate     = DATE(MONTH(TODAY), 1, YEAR(TODAY))       
       ldaToDate       = ADD-INTERVAL(ldaFromDate, 1, "months") - 1
       ldaPrevFromDate = ADD-INTERVAL(ldaFromDate, -1, "months"). /* First day of previous month */

ASSIGN
   /* set 1 second after midnight to skip STC contract activations */
   ldPeriodFrom        = Func.Common:mMake2Dt(ldaFromDate,1)   
   ldPeriodFromSTC     = Func.Common:mMake2Dt(ldaFromDate,0)   
   ldPeriodTo          = Func.Common:mMake2Dt(ldaToDate,86399)     
   lcIPLContracts      = fCParamC("IPL_CONTRACTS")
   lcBONOContracts     = fCParamC("BONO_CONTRACTS")
   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
   lcFirstMonthUsageBasedBundles = fCParamC("FIRST_MONTH_USAGE_BASED_BUNDLES")
   lcExcludeBundles    = fCParamC("EXCLUDE_BUNDLES")
   ldeStart            = Func.Common:mMakeTS()
   liPeriod            = YEAR(ldaToDate) * 100 + MONTH(ldaToDate)
   liPrevPeriod        = YEAR(ldaPrevFromDate) * 100 + MONTH(ldaPrevFromDate)
   liDayOfMonth        = DAY(TODAY).
   
      
RUN p_bundle_first_month.

ASSIGN
   ldPeriodFrom     = Func.Common:mMake2Dt(ldaFromDate,0).
       
RUN p_dss_bundle_first_month. 

/* Billed FFItem */
IF (liDayOfMonth = 1 OR liDayOfMonth = 2) THEN
DO:
   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand = "1" AND
            Invoice.InvDate >= ldaPrevFromDate:
      for-blk:
      FOR EACH FFItem NO-LOCK WHERE
               FFItem.Invnum = Invoice.InvNum:
         IF CAN-FIND(FIRST ttData WHERE
                           ttData.ffitemnum = FFItem.FFItemNum) THEN
            NEXT for-blk.
         ELSE
         DO: 
            FIND FIRST FixedFee WHERE 
                       FixedFee.FFnum = FFItem.FFNum NO-LOCK NO-ERROR.

            IF AVAIL FixedFee AND
                     FFItem.BillPeriod >= liPrevPeriod AND
                     FFItem.BillPeriod <= liPeriod THEN
            DO:           
               RUN pCreateTempData(FFItem.FFItemNum, 
                                   FixedFee.FFNum,   
                                   FFItem.CustNum, 
                                   FFItem.BillPeriod,
                                   FFItem.BillCode,  
                                   "FULL",    
                                   FixedFee.KeyValue,
                                   FixedFee.Cli,
                                   FFItem.Billed,    
                                   FFItem.amt,      
                                   FixedFee.BegDate
                                   ).
            END.
         END.
      END.
   END.
END.

FOR EACH ttData NO-LOCK:
   PUT STREAM fixfeeitem UNFORMATTED 
       ttData.FFItemNum  lcDelimiter 
       ttData.FFNum      lcDelimiter 
       ttData.custnum    lcDelimiter 
       ttData.billper    lcDelimiter 
       ttData.billcode   lcDelimiter 
       ttData.calcmethod lcDelimiter 
       ttData.msseq      lcDelimiter 
       ttData.cli        lcDelimiter 
       ttData.billed     lcDelimiter 
       ttData.feeamt     lcDelimiter 
       ttData.createdate SKIP.
   oiEvents = oiEvents + 1.
END.

ASSIGN ldeEnd   = Func.Common:mMakeTs().

OUTPUT STREAM fixfeeitem CLOSE.
SESSION:NUMERIC-FORMAT = lcNumeric.


PROCEDURE p_bundle_first_month:          

   RUN pGetAllSubscriptions.   

   RUN pCalculateFees.    

   RETURN "". 

END PROCEDURE.

PROCEDURE p_dss_bundle_first_month:

   FOR EACH Customer NO-LOCK:

      RUN pGetCustomerSubscriptions(INPUT Customer.Custnum).
      /* Unbilled FFItems */
      ffitem-loop:
      FOR EACH FFItem NO-LOCK WHERE
         FFitem.CustNum   = Customer.Custnum AND
         (IF liDayOfMonth = 1 OR liDayOfMonth = 2 THEN
             FFItem.BillPeriod >= liPrevPeriod AND
             FFItem.BillPeriod <= liPeriod
          ELSE
             FFItem.BillPeriod = liPeriod) AND         
         FFItem.billed     = NO:
         IF CAN-FIND(FIRST ttData WHERE
                           ttData.ffitemnum = FFItem.FFItemNum) THEN
            NEXT ffitem-loop.
         ELSE
         DO: 
            FIND FIRST FixedFee WHERE 
                       FixedFee.FFnum = FFItem.FFNum NO-LOCK NO-ERROR.

            IF AVAIL FixedFee THEN
            DO:              
               RUN pCreateTempData(FFItem.FFItemNum, 
                                   FixedFee.FFNum,   
                                   FFItem.CustNum, 
                                   FFItem.BillPeriod,
                                   FFItem.BillCode,  
                                   "FULL",    
                                   FixedFee.KeyValue,
                                   FixedFee.Cli,
                                   FFItem.Billed,    
                                   FFItem.amt,      
                                   FixedFee.BegDate
                                   ).
            END.
         END.
      END.
   END. /* FOR EACH Customer NO-LOCK: */

   RETURN "".

END PROCEDURE.


/******** Main end ********/


PROCEDURE pGetAllSubscriptions:

   DEF VAR liCount        AS INT  NO-UNDO. 
   DEF VAR liReqStatus    AS INT  NO-UNDO. 
   DEF VAR lcReqStatuses  AS CHAR NO-UNDO INIT "2,9".
   DEF VAR lcBundleId     AS CHAR NO-UNDO.
   DEF VAR ldaMsReqDate   AS DATE NO-UNDO.   
   
   DEF BUFFER bMsRequest FOR MsRequest.
   
   DO liCount = 1 TO NUM-ENTRIES(lcReqStatuses):

      liReqStatus = INT(ENTRY(liCount, lcReqStatuses)).

      FOR EACH MsRequest NO-LOCK WHERE
               MsRequest.Brand = Syst.Var:gcBrand AND
               MsRequest.ReqType = 8     AND
               MsRequest.ReqStat = liReqStatus    AND
               MsRequest.ActStamp >= ldPeriodFrom AND
               MsRequest.ActStamp <= ldPeriodTo,
         FIRST DayCampaign NO-LOCK WHERE
               DayCampaign.Brand = Syst.Var:gcBrand AND
               DayCampaign.DCEvent = MsRequest.ReqCParam3 AND
               LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0,
         FIRST MsOwner WHERE 
               MsOwner.MsSeq    = MsRequest.MsSeq AND  
               MsOwner.TSEnd   >= ldPeriodFrom AND
               MsOwner.TsBegin <= ldPeriodTo NO-LOCK:
       
         Func.Common:mTS2Date(INPUT  MsRequest.ActStamp,
                  OUTPUT ldaMsReqDate).

         FIND FIRST FMItem NO-LOCK WHERE
                    FMItem.Brand        = Syst.Var:gcBrand              AND
                    FMItem.FeeModel     = DayCampaign.FeeModel AND
                    FMItem.FromDate    <= ldaMsReqDate         AND
                    FMItem.ToDate      >= ldaMsReqDate         AND
                    FMItem.FirstMonthBr = 2 NO-ERROR.
          
         IF NOT AVAILABLE FMItem THEN NEXT. 
       
         /* skip first month fee calculation for contracts originating
            from normal stc/btc (not immediate STC/BTC). */
         IF MsRequest.OrigRequest > 0 THEN DO:   
            FIND FIRST bMsRequest NO-LOCK WHERE
                       bMsRequest.MsRequest = MsRequest.OrigRequest NO-ERROR.
            IF AVAIL bMsRequest AND
               bMsRequest.ActStamp = ldPeriodFromSTC AND
               (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
                bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) THEN NEXT.
         END. /* IF MsRequest.OrigRequest > 0 THEN DO: */

         lcBundleId = fGetDSSId(MsRequest.CustNum,ldPeriodTo).

         /* If this subscription is linked with DSS or DSS2 then */
         /* it will be calculated based on the DSS bundle usage  */

         IF lcBundleId = {&DSS} OR
            (lcBundleId = "DSS2" AND
             LOOKUP(MsOwner.CLIType,lcAllowedDSS2SubsType) > 0) THEN NEXT.

         RUN pCollectSubscription(MsRequest.MsSeq,
                                  MsRequest.ReqCParam3).

      END. /* FOR EACH MsRequest NO-LOCK WHERE */
   END. /* DO liCount = 1 TO NUM-ENTRIES(lcReqStatuses): */
   
   RETURN "". 

END PROCEDURE. 
                               
PROCEDURE pCollectSubscription:

   DEF INPUT PARAMETER iiMsSeq   AS INT  NO-UNDO.
   DEF INPUT PARAMETER icDCEvent AS CHAR NO-UNDO.    
   
   FOR EACH ServiceLimit NO-LOCK WHERE 
            ServiceLimit.GroupCode = icDCEvent,
      /* packages that were activated (and not terminated) on this period */
       FIRST MServiceLimit NO-LOCK WHERE
             MServiceLimit.MsSeq = iiMsSeq AND
             MServiceLimit.SlSeq = ServiceLimit.SlSeq AND
             MServiceLimit.FromTS >= ldPeriodFrom AND
             MServiceLimit.FromTS <= ldPeriodTo AND
             MServiceLimit.EndTS >= ldPeriodTo:

      /* only bundles */
      IF NOT fIsBundle(ServiceLimit.GroupCode) THEN LEAVE.    

      /* First month always usage based */
      IF LOOKUP(icDCEvent,lcFirstMonthUsageBasedBundles) > 0 THEN .

      /* If bundle is terminated because of BTC in first month */
      /* then bundle will be charged based on the usage        */
      ELSE IF CAN-FIND(FIRST MSRequest WHERE
                        MSRequest.MsSeq = iiMsSeq AND
                        MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ActStamp = ldPeriodTo AND
                        MsRequest.ReqCParam3 = icDCEvent AND
                        MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
          LOOKUP(STRING(MsRequest.ReqStat),"4,99") = 0
                  USE-INDEX MsSeq) OR
         CAN-FIND(FIRST MSRequest WHERE
                        MSRequest.MsSeq = iiMsSeq AND
                        MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ActStamp = ldPeriodTo AND
                        MsRequest.ReqCParam3 = icDCEvent AND
                        LOOKUP(MsRequest.ReqCParam3,lcBONOContracts) = 0 AND
                        LOOKUP(MsRequest.ReqCParam3,lcIPLContracts)  = 0 AND
                        MsRequest.ReqSource  = {&REQUEST_SOURCE_BTC}
                        USE-INDEX MsSeq)
      THEN LEAVE.
      
      CREATE ttSubscription.
      ASSIGN
         ttSubscription.MsSeq        = iiMsSeq 
         ttSubscription.ServiceLimit = ServiceLimit.GroupCode.              
      
      LEAVE.
      
   END.

END PROCEDURE.

PROCEDURE pCalculateFees:

   DEF VAR ldFeeAmount AS DEC  NO-UNDO.         

   FOR EACH ttSubscription,
      FIRST DayCampaign NO-LOCK WHERE
            DayCampaign.Brand   = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = ttSubscription.ServiceLimit,
      FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
            FixedFee.Brand     = Syst.Var:gcBrand AND
            FixedFee.HostTable = "MobSub" AND
            FixedFee.KeyValue  = STRING(ttSubscription.MsSeq) AND
            FixedFee.FeeModel  = DayCampaign.FeeModel AND
            FixedFee.CalcObj   = DayCampaign.DCEvent AND
            FixedFee.InUse     = TRUE AND
            FixedFee.BegDate  >= ldaFromDate AND
            FixedFee.BegDate  <= ldaToDate AND
            FixedFee.EndPer   >= liPeriod,
      FIRST FMItem NO-LOCK WHERE
            FMItem.Brand     = Syst.Var:gcBrand AND
            FMItem.FeeModel  = FixedFee.FeeModel AND
            FMItem.FromDate <= FixedFee.BegDate AND
            FMItem.ToDate   >= FixedFee.BegDate AND
            FMItem.FirstMonthBr = 2:
      
      FIND FIRST FFItem WHERE 
                 FFItem.FFNum      = FixedFee.FFNum AND                                  
                 FFItem.BillPeriod = liPeriod NO-LOCK NO-ERROR.  
                                       
      IF NOT AVAIL FFItem OR FFItem.Billed THEN NEXT.                        
             
      IF NOT CAN-FIND(FIRST ttData WHERE
                            ttData.FFItemNum  = FFItem.FFItemNum) THEN
      DO:           
         ldFeeAmount = fCalculateFirstMonthFee(Syst.Var:gcBrand,
                                               ttSubscription.MsSeq,
                                               ttSubscription.ServiceLimit,
                                               FixedFee.Amt,
                                               liPeriod).      

         RUN pCreateTempData(FFItem.FFItemNum, 
                             FixedFee.FFNum,   
                             FFItem.CustNum, 
                             FFItem.BillPeriod,
                             FFItem.BillCode,  
                             "CALCULATED",    
                             FixedFee.KeyValue,
                             FixedFee.Cli,
                             FFItem.Billed,    
                             ldFeeAmount,      
                             FixedFee.BegDate
                            ).                 
      END.      
   END.      

   RETURN "".

END PROCEDURE.

PROCEDURE pGetCustomerSubscriptions:

   DEF INPUT PARAMETER iiInvCust    AS INT  NO-UNDO.

   DEF VAR ldeDSSUsage              AS DEC  NO-UNDO.
   DEF VAR ldeBundleAmt             AS DEC  NO-UNDO.
   DEF VAR ldeDSSLimit              AS DEC  NO-UNDO.
   DEF VAR llFullMonth              AS LOG  NO-UNDO.
   DEF VAR ldFFItemStartDate        AS DATE NO-UNDO.
   DEF VAR liFFItemStartTime        AS INT  NO-UNDO.
   DEF VAR ldFeeAmount              AS DEC  NO-UNDO.
   DEF VAR lcBundleId               AS CHAR NO-UNDO.

   DEF BUFFER bMServiceLimit        FOR MServiceLimit.
   DEF BUFFER bServiceLimit         FOR ServiceLimit.
   DEF BUFFER bDayCampaign          FOR DayCampaign.
   DEF BUFFER bMSRequest            FOR MsRequest.
   DEF BUFFER MSRequest             FOR MsRequest.
   DEF BUFFER b2MSRequest           FOR MsRequest.
   DEF BUFFER bFixedFee             FOR FixedFee.

   EMPTY TEMP-TABLE ttSub NO-ERROR.
   EMPTY TEMP-TABLE ttMsOwner NO-ERROR.

   /* Check wheather customer is linked with DSS service or not */
   lcBundleId = fGetDSSId(iiInvCust,ldPeriodTo).
   IF lcBundleId = "" THEN RETURN.

   ldeDSSUsage = fGetDSSUsage(INPUT iiInvCust,
                              INPUT ldaFromDate,
                              OUTPUT ldeDSSLimit).

   fGetMsOwnerTempTable(iiInvCust,ldaFromDate,ldaToDate,FALSE,FALSE).

   FOR EACH ttMsOwner BREAK BY ttMsOwner.MsSeq:

      IF lcBundleId = "DSS2" AND
         LOOKUP(ttMsOwner.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.

      DATA_BUNDLE_LOOP:
      FOR EACH bMServiceLimit WHERE
               bMServiceLimit.MsSeq    = ttMsOwner.MsSeq AND
               bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
               bMServiceLimit.FromTS  <= ttMsOwner.PeriodTo AND
               bMServiceLimit.EndTS   >= ttMsOwner.PeriodFrom NO-LOCK,
         FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
               bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
         FIRST bDayCampaign NO-LOCK WHERE
               bDayCampaign.Brand = Syst.Var:gcBrand AND
               bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
               LOOKUP(bDayCampaign.DCType,
                     {&PERCONTRACT_RATING_PACKAGE}) > 0:

         IF LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 OR
            bDayCampaign.DCEvent BEGINS {&DSS}
         THEN NEXT DATA_BUNDLE_LOOP.

         IF CAN-FIND(FIRST ttSub WHERE
                           ttSub.MsSeq    = bMServiceLimit.MsSeq AND
                           ttSub.BundleId = bServiceLimit.GroupCode)
         THEN NEXT DATA_BUNDLE_LOOP.

         ASSIGN ldeBundleAmt = (bMServiceLimit.InclAmt * 1024 * 1024)
                llFullMonth  = FALSE.

         /* If ongoing bundle termination request OR NOT FIRST  */
         /* month then reduce data amount from total DSS limit  */
         /* because bundle will be charged as full month amt    */

         /* First and last month is always usage based          */
         IF LOOKUP(bDayCampaign.DCEvent,lcFirstMonthUsageBasedBundles) > 0 AND
            (bMServiceLimit.FromTs >= ldPeriodFrom OR
             bMServiceLimit.EndTs < ldPeriodTo) THEN .

         ELSE IF bMServiceLimit.FromTS < ldPeriodFrom THEN
            llFullMonth = TRUE.

         /* If bundle is terminated because of BTC in first month */
         /* then bundle will be charged based on the usage        */
         ELSE IF CAN-FIND(FIRST MSRequest WHERE
            MSRequest.MsSeq = bMServiceLimit.MsSeq AND
            MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
            MsRequest.ActStamp >= ldPeriodFrom AND
            MsRequest.ActStamp <= ldPeriodTo AND
            MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
            MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
            LOOKUP(STRING(MsRequest.ReqStat),"4,99") = 0 USE-INDEX MsSeq) OR
            CAN-FIND(FIRST MSRequest WHERE
            MSRequest.MsSeq = bMServiceLimit.MsSeq AND
            MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
            MsRequest.ActStamp >= ldPeriodFrom AND
            MsRequest.ActStamp <= ldPeriodTo AND
            MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
            LOOKUP(MsRequest.ReqCParam3,lcBONOContracts) = 0 AND
            LOOKUP(MsRequest.ReqCParam3,lcIPLContracts)  = 0 AND
            MsRequest.ReqSource  = {&REQUEST_SOURCE_BTC} USE-INDEX MsSeq)
         THEN llFullMonth = TRUE.
            
         /* if bundle is originate from stc or btc then it as full month */
         IF llFullMonth = FALSE AND
            bMServiceLimit.FromTs >= ldPeriodFrom THEN
            FOR EACH MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = ttMsOwner.MsSeq AND
                     MsRequest.ReqType = 8 AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     MsRequest.ActStamp >= ldPeriodFrom AND
                     MsRequest.ActStamp <= ldPeriodTo AND
                     MsRequest.OrigRequest > 0 AND
                LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0 USE-INDEX MsSeq,
               FIRST bMsRequest NO-LOCK WHERE
                     bMsRequest.MsRequest = MsRequest.OrigRequest:
               IF (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
                   bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) AND
                   bMsRequest.ActStamp = ldPeriodFrom THEN DO:
                  llFullMonth = TRUE.
                  LEAVE.
               END. /* IF bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} */
            END. /* FOR EACH MsRequest NO-LOCK WHERE */

         /* YTS-6939  CONT15, Activation >= 6.5.2015
             Termination month = activation month
             DSS active, CONT15 data counter not full */
         IF llFullMonth = FALSE AND 
            (bMServiceLimit.FromTs >= ldPeriodFrom OR
             bMServiceLimit.EndTs <= ldPeriodTo) AND
            (bDayCampaign.DCType = {&DCTYPE_SERVICE_PACKAGE} OR
             bDayCampaign.DCType = {&DCTYPE_BUNDLE}) THEN DO:

            FIND FIRST MSRequest NO-LOCK WHERE
                       MSRequest.MsSeq = ttMsOwner.MsSeq AND
                       MSRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                       MsRequest.ActStamp >= ldPeriodFrom AND
                       MsRequest.ActStamp <= ldPeriodTo AND
                       MSRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                       MsRequest.ReqSource  = {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} AND
                   LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0 USE-INDEX MsSeq NO-ERROR.
            IF AVAIL MsRequest THEN DO:
            
               FIND FIRST MobSub NO-LOCK WHERE
                          MobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
               IF AVAILABLE MobSub THEN
                  llFullMonth = (MobSub.ActivationDate >= 5/6/2015).
               ELSE DO:
                  FIND FIRST TermMobSub NO-LOCK WHERE
                             TermMobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
                  IF AVAILABLE TermMobSub THEN
                     llFullMonth= (TermMobSub.ActivationDate >= 5/6/2015).
               END.
            END.
         END.

         /* if the data bundle contract is terminated from STC/BTC
            in middle of month then skip first month fee calculation */
         IF llFullMonth = TRUE AND
            bMServiceLimit.EndTs < ldPeriodTo THEN
            FOR EACH MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = ttMsOwner.MsSeq AND
                     MsRequest.ReqType = 9 AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     MsRequest.ActStamp >= ldPeriodFrom AND
                     MsRequest.ActStamp < ldPeriodTo AND
                     MsRequest.OrigRequest > 0 AND
                     LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0
                     USE-INDEX MsSeq,
               FIRST bMsRequest NO-LOCK WHERE
                     bMsRequest.MsRequest = MsRequest.OrigRequest:
               IF (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
                   bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) AND
                   bMsRequest.ActStamp < ldPeriodTo THEN DO:
                  llFullMonth = FALSE.
                  LEAVE.
               END.
            END.


         IF llFullMonth THEN DO:
            IF ldeDSSUsage >= ldeBundleAmt THEN
               ldeDSSUsage = (ldeDSSUsage - ldeBundleAmt).
            ELSE ldeDSSUsage = 0.

            NEXT DATA_BUNDLE_LOOP.
         END. /* IF llFullMonth THEN DO: */

         CREATE ttSub.
         ASSIGN ttSub.MsSeq           = bMServiceLimit.MsSeq
                ttSub.CustNum         = ttMsOwner.CustNum
                ttSub.BundleId        = bServiceLimit.GroupCode
                ttSub.BundleLimit     = ldeBundleAmt
                ttSub.BundleLimitInMB = bMServiceLimit.InclAmt
                ttSub.InclUnit        = bServiceLimit.InclUnit
                .

      END. /* FOR EACH bMServiceLimit WHERE */
   END. /* FOR EACH ttMsOwner NO-LOCK WHERE */


   /* Calculate first month fee */
   FOR EACH ttSub WHERE
            ttSub.CustNum = iiInvCust:       

      ldFeeAmount = 0.

      FOR FIRST DayCampaign NO-LOCK WHERE
                DayCampaign.Brand   = Syst.Var:gcBrand AND
                DayCampaign.DCEvent = ttSub.BundleId,
          FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
                FixedFee.Brand     = Syst.Var:gcBrand AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue  = STRING(ttSub.MsSeq) AND
                FixedFee.FeeModel  = DayCampaign.FeeModel AND
                FixedFee.CalcObj   = DayCampaign.DCEvent AND
                FixedFee.InUse     = TRUE AND
                FixedFee.BegDate  <= ldaToDate AND
                FixedFee.EndPer   >= liPeriod,
          FIRST FMItem NO-LOCK WHERE
                FMItem.Brand     = Syst.Var:gcBrand AND
                FMItem.FeeModel  = FixedFee.FeeModel AND
                FMItem.FromDate <= FixedFee.BegDate AND
                FMItem.ToDate   >= FixedFee.BegDate AND
                FMItem.FirstMonthBr = 2:

          /* already billed */
          FIND FIRST FFItem OF FixedFee WHERE
                     FFItem.BillPeriod = liPeriod NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FFItem OR FFItem.Billed THEN NEXT.          
      
          IF NOT CAN-FIND(FIRST ttData WHERE
                                ttData.FFItemNum  = FFItem.FFItemNum) THEN
          DO:                        
             /* % of fee, based on usage */
             IF ldeDSSUsage > 0 THEN
                ldFeeAmount = fCalculateProportionalFee(
                                            (IF ldeDSSUsage < ttSub.BundleLimit
                                             THEN ldeDSSUsage
                                             ELSE ttSub.BundleLimit),
                                             ttSub.InclUnit,
                                             ttSub.BundleLimitInMB,
                                             FixedFee.Amt).
                        
             ELSE ldFeeAmount = 0.

             RUN pCreateTempData(FFItem.FFItemNum, 
                                 FixedFee.FFNum,   
                                 FixedFee.CustNum, 
                                 FFItem.BillPeriod,
                                 FFItem.BillCode,  
                                 "DSSCALCULATED",    
                                 FixedFee.KeyValue,
                                 FixedFee.Cli,
                                 FFItem.Billed,    
                                 ldFeeAmount,      
                                 FixedFee.BegDate
                                ).                
          END.                                
      END. /* FOR FIRST DayCampaign NO-LOCK WHERE */

      IF ldeDSSUsage > 0 THEN
         ldeDSSUsage = (ldeDSSUsage - ttSub.BundleLimit).

   END. /* FOR EACH ttSub BREAK BY ttSub.CustNum: */

   RETURN "".

END PROCEDURE.

PROCEDURE pCreateTempData:
   DEFINE INPUT PARAMETER iFFItemNum  AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER iFFNum      AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER iCustNum    AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER iBillPer    AS INTEGER   NO-UNDO. 
   DEFINE INPUT PARAMETER cBillCode   AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER cCalcMethod AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER cKeyvalue   AS CHARACTER NO-UNDO.     
   DEFINE INPUT PARAMETER cCli        AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER lBilled     AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER dFeeAmt     AS DECIMAL   NO-UNDO.   
   DEFINE INPUT PARAMETER dtBegDate   AS DATE      NO-UNDO. 

   CREATE ttData.
   ASSIGN ttData.FFItemNum    = iFFItemNum
          ttData.FFNum        = iFFNum
          ttData.custnum      = iCustNum
          ttData.billper      = iBillPer
          ttData.billcode     = cBillCode
          ttData.calcmethod   = cCalcMethod
          ttData.msseq        = cKeyvalue
          ttData.cli          = cCli
          ttData.billed       = lBilled
          ttData.feeamt       = dFeeAmt
          ttData.createdate   = dtBegDate.

END PROCEDURE.




