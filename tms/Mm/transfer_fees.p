/*------------------------------------------------------------------------
    File        : transfer_fees.p
    Purpose     : transfer fees from FixedLine to MobileLine
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : Mon Jun 11 12:10:04 IST 2018
    Notes       : as part of 2p3pMerge
  ----------------------------------------------------------------------*/

{Syst/eventval.i}
{Syst/tmsconst.i} 
{Func/msreqfunc.i}
{Func/fmakemsreq.i}
{Func/fsubstermreq.i}
{Func/nncoit2.i }
{Func/msisdn_prefix.i}
{Func/create_eventlog.i}
{Func/fsubstermreq.i}

Syst.Var:gcBrand = "1".

DEFINE BUFFER mobileLine FOR MobSub.
DEFINE BUFFER fixedLine  FOR MobSub.
DEFINE BUFFER oldMLOwner FOR MSOwner.
DEFINE BUFFER oldFLOwner FOR MSOwner.
DEFINE BUFFER subReq     FOR MsRequest.
DEFINE BUFFER mlFFee     FOR fixedFee.
DEFINE BUFFER flFFee     FOR fixedFee.

FUNCTION fgetCliTypeName RETURNS CHARACTER (INPUT CHARACTER) FORWARD.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobSub AS HANDLE NO-UNDO.
   lhMobSub = BUFFER mobileLine:HANDLE.
   RUN StarEventInitialize(lhMobSub).

   DEFINE VARIABLE lhMSOwner AS HANDLE NO-UNDO.
   lhMSOwner = BUFFER oldMLOwner:HANDLE.
   RUN StarEventInitialize(lhMSOwner).
END.

DEF TEMP-TABLE ttContract NO-UNDO
   FIELD DCEvent   AS CHAR
   FIELD PerContID AS INT
   FIELD CreateFee AS LOG
   FIELD ActTS     AS DEC.


DEFINE INPUT PARAMETER iiMsRequestId AS INTEGER   NO-UNDO.

DEFINE VARIABLE liTerminate      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcError          AS CHARACTER NO-UNDO.

DEFINE VARIABLE llYoigoCLI       AS LOG       NO-UNDO.
DEFINE VARIABLE llMasmovilCLI    AS LOG       NO-UNDO.
DEFINE VARIABLE piMsisdnStat     AS INTEGER   NO-UNDO.
DEFINE VARIABLE piSimstat        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTenant         AS CHARACTER NO-UNDO.
DEFINE VARIABLE piQuarTime       AS INTEGER   NO-UNDO.
DEFINE VARIABLE llYoigoTenant    AS LOG       NO-UNDO INIT FALSE.
DEFINE VARIABLE llMasmovilTenant AS LOG       NO-UNDO INIT FALSE.
DEFINE VARIABLE lcFFNums         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedNumber    AS CHARACTER NO-UNDO.

DO ON ERROR UNDO , LEAVE :
    
    FIND MsRequest WHERE MSRequest.MsRequest = iiMsRequestId NO-LOCK NO-ERROR.
    IF NOT AVAILABLE MsRequest THEN 
        RETURN "Invalid MsRequest".

    IF fChkSubRequest(iiMsRequestId) AND 
       CAN-FIND(FIRST subreq WHERE SubReq.OrigRequest = iiMsRequestId 
              AND subreq.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION}) THEN DO:
        FIND ActionLog WHERE 
             ActionLog.Brand        = Syst.Var:gcBrand       AND    
             ActionLog.TableName    = "MsRequest"            AND 
             ActionLog.KeyValue     = STRING(iiMsRequestId)  AND 
             ActionLog.ActionID     = "FixedFees" NO-LOCK NO-ERROR.
        IF AVAILABLE ActionLog THEN DO TRANSACTION: 
            FIND FixedFee WHERE 
                 FixedFee.FFNum = INTEGER(ActionLog.ActionChar) 
                 EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE FixedFee THEN 
                FixedFee.Active = YES.
            RELEASE FixedFee.
        END.  
        fReqStatus( {&REQUEST_STATUS_DONE},"").
        RETURN. 
    END.
    
    FIND CLIType WHERE CLIType.Clitype = MSRequest.ReqCParam1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CLIType THEN 
    DO:
        fReqError("Invalid CliType.").
        RETURN.
    END.
    IF CLIType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY} THEN 
    DO: 
        FIND fixedLine NO-LOCK WHERE fixedLine.MsSeq = MSRequest.MsSeq NO-ERROR.
        FIND mobileLine NO-LOCK WHERE 
             mobileLine.Cli = ENTRY(1, MSRequest.reqCparam3 ,"|") NO-ERROR.
    END.
    ELSE IF CLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN 
    DO:           
        FIND mobileLine NO-LOCK WHERE 
             mobileLine.MsSeq = MSRequest.MsSeq NO-ERROR.
        FIND fixedLine NO-LOCK WHERE 
             fixedLine.Cli = ENTRY(1, MSRequest.reqCparam3 ,"|") NO-ERROR.
    END.
    ELSE 
    DO:
        fReqError("Invalid TariffType").
        RETURN.
    END.
    
    IF NOT AVAILABLE mobileLine OR NOT AVAILABLE fixedLine THEN 
    DO:
        IF NOT AVAILABLE mobileLine THEN 
            fReqError("Mobile line not found.").
        ELSE 
            fReqError("Fixed line not found.").
        RETURN.
    END.
    
    IF CAN-FIND(FIRST subReq WHERE 
             subReq.OrigRequest = iiMsRequestId AND 
             subReq.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND 
             subReq.ReqStatus = {&REQUEST_STATUS_DONE}) THEN DO:
        RUN pCreateTermRequest.
        RETURN.
    END.

    IF CAN-FIND(FIRST subReq WHERE 
                      subReq.OrigRequest = iiMsRequestId AND 
                      subReq.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND 
                      subReq.reqstatus = {&REQUEST_STATUS_HOLD} ) THEN 
        DO TRANSACTION ON ERROR UNDO , THROW:
            
            FOR EACH flFFee NO-LOCK WHERE 
                flFFee.Brand = Syst.Var:gcBrand AND
                flFFee.HostTable = 'mobsub' AND 
                flFFee.KeyValue = STRING(fixedLine.MsSeq) AND 
                flFFee.BillCode BEGINS "PAYTERM" :
                CREATE mlFFee.
                BUFFER-COPY flFFee TO mlFFee 
                    ASSIGN 
                    mlFFee.FFNum       = NEXT-VALUE(Contract)
                    mlFFee.HostTable   = 'mobsub'
                    mlFFee.KeyValue    = STRING(mobileLine.MsSeq)
                    mlFFee.SourceTable = 'FixedFee'
                    mlFFee.SourceKey   = STRING(flFFee.FFNum) 
                    mlFFee.Active      = FALSE . 
                fMakeContractMore(mlFFee.FFNum  , mlFFee.endPeriod) .
                lcFFNums         = lcFFNums  + "," + STRING(mlFFee.FFNum) .
            END.
            lcFFNums = TRIM(lcFFNums).
    
            CREATE ActionLog.
            ASSIGN 
                ActionLog.Brand        = Syst.Var:gcBrand   
                ActionLog.TableName    = "MsRequest"  
                ActionLog.KeyValue     = STRING(iiMsRequestId)
                ActionLog.UserCode     = 'Newton'
                ActionLog.ActionID     = "FixedFees"
                ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
                ActionLog.ActionChar   = lcFFNums
                ActionLog.CustNum      = MSRequest.Custnum
                ActionLog.ActionTS     = Func.Common:mMakeTS().        
            
            FIND CURRENT mobileLine EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            FIND CURRENT fixedLine  EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            
            FIND FIRST oldMLOwner EXCLUSIVE-LOCK 
                 WHERE oldMLOwner.MSSEQ  = mobileLine.MSSeq  
                   AND oldMLOwner.CLI    = mobileLine.CLI  
                   AND oldMLOwner.TSEND >= Func.Common:mMakeTS() NO-ERROR.
            
            FIND FIRST oldFLOwner EXCLUSIVE-LOCK 
                 WHERE oldFLOwner.MSSEQ  = fixedLine.MSSeq  
                   AND oldFLOwner.CLI    = fixedLine.CLI  
                   AND oldFLOwner.TSEND >= Func.Common:mMakeTS() NO-ERROR.
            
            IF llDoEvent THEN DO:
                lhMSOwner = BUFFER oldMLOwner:HANDLE. 
                RUN StarEventSetOldBuffer(lhMSOwner).
                oldMLOwner.TsEnd       = Func.Common:mMakeTS().
                RUN StarEventMakeModifyEvent(lhMSOwner).
                lhMSOwner = BUFFER oldFLOwner:HANDLE. 
                RUN StarEventSetOldBuffer(lhMSOwner).
                oldFLOwner.TsEnd       = Func.Common:mMakeTS().
                RUN StarEventMakeModifyEvent(lhMSOwner).
            END.
            ELSE 
                ASSIGN 
                    oldMLOwner.TsEnd       = Func.Common:mMakeTS()
                    oldFLOwner.TsEnd       = Func.Common:mMakeTS().

            IF llDoEvent THEN DO:
                lcFixedNumber = fixedLine.FixedNumber. 
                lhMobSub = BUFFER fixedLine:HANDLE. 
                RUN StarEventSetOldBuffer(lhMobSub).
                fixedLine.FixedNumber  = ? .
                RUN StarEventMakeModifyEvent(lhMobSub).

                lhMobSub = BUFFER mobileLine:HANDLE. 
                RUN StarEventSetOldBuffer(lhMobSub).
                mobileLine.FixedNumber = lcFixedNumber.
                RUN StarEventMakeModifyEvent(lhMobSub).
            END.
            ELSE 
                ASSIGN
                    lcFixedNumber = fixedLine.FixedNumber  
                    fixedLine.FixedNumber  = ? 
                    mobileLine.FixedNumber = lcFixedNumber .

            CREATE MSOwner.
            BUFFER-COPY oldMLOwner TO Msowner
            ASSIGN
                MSOwner.TsBegin     = Func.Common:mMakeTS()
                MSOWner.TSEnd       = 99999999.99999
                MsOwner.CLIEvent    = "FixedNumber"
                MSOwner.FixedNumber = mobileLine.FixedNumber .
                
            IF llDoEvent THEN fMakeCreateEvent(
                 (BUFFER MsOwner:HANDLE),
                 "",
                 Syst.Var:katun,
                 "").                

            CREATE MSOwner.
            BUFFER-COPY oldFLOwner TO Msowner
            ASSIGN
                MSOwner.TsBegin     = Func.Common:mMakeTS()
                MSOWner.TSEnd       = 99999999.99999
                MsOwner.CLIEvent    = "FixedNumber"
                MSOwner.FixedNumber = fixedLine.FixedNumber .
                
            IF llDoEvent THEN fMakeCreateEvent(
                 (BUFFER MsOwner:HANDLE),
                 "",
                 Syst.Var:katun,
                 "").                
            
            FIND Order WHERE 
                 Order.Brand = Syst.Var:gcBrand AND 
                 Order.MSSeq = fixedline.MsSeq  NO-LOCK NO-ERROR.

            IF NOT AVAILABLE order THEN
                UNDO, THROW NEW Progress.Lang.AppError 
                               ( "Order not found for fixedline" , 2 ). 
            
            CREATE ActionLog.
            ASSIGN 
                ActionLog.Brand        = Syst.Var:gcBrand    
                ActionLog.TableName    = "FixedNumber"  
                ActionLog.KeyValue     = mobileLine.FixedNumber
                ActionLog.UserCode     = 'Newton'
                ActionLog.ActionID     = "OrderId"
                ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
                ActionLog.ActionChar   = STRING(order.OrderID)
                ActionLog.CustNum      = MSRequest.Custnum
                ActionLog.ActionTS     = Func.Common:mMakeTS().   
                                
            RELEASE mobileLine.
            RELEASE fixedLine.
            RELEASE MSOwner.
            
            FIND subReq WHERE 
                 subReq.OrigRequest = iiMsRequestId AND 
                 subReq.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} 
                 EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    
            IF NOT AVAILABLE SubReq THEN
                UNDO , THROW NEW Progress.Lang.AppError 
                       ( "STC request not found " , 1 ). 
            
            subReq.ReqStatus = {&REQUEST_STATUS_NEW} .
            subReq.Mandatory = 1 .
            
            fReqStatus( {&REQUEST_STATUS_SUB_REQUEST_PENDING}, "") .
            
            CATCH err AS Progress.Lang.Error :
                fReqError("Error occured : " + err:GetMessage(1)).
            END CATCH.  
        END.
END.

FINALLY:
    fCleanEventObjects().	
END FINALLY.

PROCEDURE pCreateTermRequest:
    
  DEFINE VARIABLE ldCurrTS           AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE llCreateFee        AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE liTermRequest      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lcError            AS CHARACTER NO-UNDO.
  DEFINE VARIABLE liMsisdnStat       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE liSimStat          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE liQuarTime         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE ldeSMSStamp        AS DECIMAL   NO-UNDO. 
  DEFINE VARIABLE ldaSecSIMTermDate  AS DATE      NO-UNDO.
  DEFINE VARIABLE ldeSecSIMTermStamp AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lcMemoString       AS CHARACTER NO-UNDO.
  DEFINE BUFFER subReq FOR MSRequest.
  DEFINE BUFFER bCliType FOR CLIType.
  
  IF CAN-FIND(FIRST subreq WHERE SubReq.OrigRequest = iiMsRequestId 
                AND subreq.ReqType = {&REQTYPE_CONTRACT_TERMINATION} ) THEN DO:
      IF fChkSubRequest(iiMsRequestId) AND 
         NOT CAN-FIND(FIRST subreq WHERE SubReq.OrigRequest = iiMsRequestId 
              AND subreq.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION}) THEN DO:
       
          FIND FIRST Order WHERE
            Order.Brand = Syst.var:gcBrand AND  
            Order.MSSeq = Fixedline.MSSeq NO-LOCK NO-ERROR.
          IF AVAILABLE Order THEN DO:
              lcMemoString = " &1: &2 (Order ID: &3) + &4: &5 --> &6 (&7 ) ".
              lcMemoString = SUBSTITUTE (lcMemoString ,
                                         fixedline.Cli ,
                                         fgetCliTypeName ( fixedline.CliType ),
                                         STRING(Order.OrderId),
                                         Mobileline.Cli ,
                                         fgetCliTypeName ( mobileline.CliType ),
                                         fgetCliTypeName(MSRequest.ReqCParam2 ),
                                         STRING(TODAY,"99-99-9999")).
          END.
          ELSE 
            ASSIGN 
              lcMemoString = " &1: &2 + &4: &5 --> &6 (&7 ) "
              lcMemoString = SUBSTITUTE (lcMemoString ,
                                         fixedline.Cli ,
                                         fgetCliTypeName ( fixedline.CliType ),
                                         '',
                                         Mobileline.Cli ,
                                         fgetCliTypeName ( mobileline.CliType ),
                                         fgetCliTypeName(MSRequest.ReqCParam2 ),
                                         STRING(TODAY,"99-99-9999")).
                      
          Func.Common:mWriteMemo("MobSub",
                               STRING(fixedline.MsSeq),
                               fixedline.Custnum,
                               "Tariff Change",
                               lcMemoString ).
          Func.Common:mWriteMemo("MobSub",
                               STRING(mobileline.MsSeq),
                               mobileline.Custnum,
                               "Tariff Change",
                               lcMemoString ).
                  
          ASSIGN 
             ldaSecSIMTermDate  = ADD-INTERVAL(TODAY, 1,"months")
             ldaSecSIMTermDate  = Func.Common:mLastDayOfMonth(ldaSecSIMTermDate)
             ldeSecSIMTermStamp = Func.Common:mMake2DT(ldaSecSIMTermDate,86399).
          fInitialiseValues(
            {&SUBSCRIPTION_TERM_REASON_MULTISIM},
            fIsYoigoCLI(fixedline.CLI), 
            fIsMasmovilCLI(fixedline.CLI),
            OUTPUT liMsisdnStat,
            OUTPUT liSimStat,
            OUTPUT liQuarTime).
            
           liTermRequest = fTerminationRequest(
                           fixedline.Msseq,
                           ldeSecSIMTermStamp,
                           liMsisdnStat,
                           liSimStat,
                           liQuarTime,
                           1, /* create fees */
                           "", /* out oper. */
                           STRING({&SUBSCRIPTION_TERM_REASON_MULTISIM}),
                           {&REQUEST_SOURCE_MERGE_TRANSFER_FEES},
                           Syst.Var:katun,
                           iiMsRequestId, /* orig. request */
                           {&TERMINATION_TYPE_FULL},
                           OUTPUT lcError).
           FIND MsRequest WHERE 
                MSRequest.MsRequest = iiMsRequestId NO-LOCK NO-ERROR.
           IF lcError > "" THEN 
               fReqStatus( {&REQUEST_STATUS_REJECTED}, 
                           "Subscription termination failed") .
           ELSE 
               fReqStatus( {&REQUEST_STATUS_SUB_REQUEST_PENDING}, "") .
      END.
      RETURN.
  END.
    
  FOR EACH DCCLI EXCLUSIVE-LOCK 
     WHERE DCCLI.MsSeq = fixedline.MsSeq  
       AND DCCLI.ValidTo >= TODAY:
         
      FIND FIRST DayCampaign NO-LOCK 
           WHERE DayCampaign.Brand = Syst.Var:gcBrand  
             AND DayCampaign.DcEvent = DCCLI.DcEvent NO-ERROR.
      
      DCCLI.TermDate = ?.
      
      IF CAN-FIND( FIRST ttContract WHERE
                   ttContract.DCEvent   = DCCLI.DCEvent ) THEN NEXT.

      CREATE ttContract.
      ASSIGN
         ttContract.DCEvent   = DCCLI.DCEvent
         ttContract.CreateFee = DCCLI.CreateFee
         ttContract.PerContID = (IF AVAIL DayCampaign AND
                                   DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT}
                                THEN DCCLI.PerContractID 
                                ELSE 0).
   END.    
    
   ldCurrTS = Func.Common:mMakeTS().
   
   FOR EACH MServiceLimit EXCLUSIVE-LOCK 
      WHERE MServiceLimit.MsSeq = fixedline.MsSeq 
        AND MServiceLimit.EndTS > ldCurrTS, 
      FIRST ServiceLimit NO-LOCK USE-INDEX SlSeq 
      WHERE ServiceLimit.SLSeq = MServiceLimit.SLSeq:

      FIND FIRST DayCampaign  
           WHERE DayCampaign.Brand      = Syst.Var:gcBrand  
             AND DayCampaign.DCEvent    = ServiceLimit.GroupCode  
             AND DayCampaign.ValidFrom <= TODAY  
             AND DayCampaign.ValidTo   >= TODAY NO-LOCK NO-ERROR.
              
      IF AVAILABLE DayCampaign THEN DO:
         IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 AND
            STRING(DayCampaign.DCType) <> {&DCTYPE_CUMULATIVE_RATING}
         THEN DO:
            FIND FIRST ttContract WHERE
                       ttContract.DCEvent = ServiceLimit.GroupCode NO-ERROR.
            IF NOT AVAILABLE ttContract THEN DO:
               CREATE ttContract.
               ASSIGN
                  ttContract.DCEvent   = ServiceLimit.GroupCode
                  ttContract.CreateFee = (DayCampaign.TermFeeModel > "").
               END.
            END.
      END.
      ELSE DO:
         MServiceLimit.EndTS = ldCurrTS.
      END.
   END.

   FOR EACH ttContract:

      FIND FIRST DayCampaign WHERE 
                 DayCampaign.Brand      = Syst.Var:gcBrand   AND 
                 DayCampaign.DCEvent    = ttContract.DCEvent AND 
                 DayCampaign.ValidTo   >= TODAY NO-LOCK NO-ERROR.
              
      IF NOT AVAIL DayCampaign THEN DO:
         NEXT.
      END.

      llCreateFee = FALSE.

      liTermRequest = 
         fPCActionRequest(fixedLine.MsSeq,  
                          ttContract.DCEvent,
                          "term",
                          IF ttContract.ActTS > 0
                          THEN ttContract.ActTS
                          ELSE Func.Common:mSecOffSet(ldCurrTS,60),
                          llCreateFee,             /* create fees */
                          {&REQUEST_SOURCE_MERGE_TRANSFER_FEES},
                          "",
                          iiMsRequestId,
                          TRUE,
                          "",
                          0,
                          ttContract.PerContID,
                          "",
                          OUTPUT lcError).

      DELETE ttContract.
   END.
   
   IF CAN-FIND(FIRST subreq WHERE 
                     SubReq.OrigRequest = iiMsRequestId AND 
                     subreq.ReqType = {&REQTYPE_CONTRACT_TERMINATION} ) THEN DO:
        FIND MsRequest WHERE 
             MSRequest.MsRequest = iiMsRequestId NO-LOCK NO-ERROR.
        fReqStatus( {&REQUEST_STATUS_SUB_REQUEST_PENDING}, "") .
   END.
      
END PROCEDURE.     

FUNCTION fgetCliTypeName  RETURNS CHARACTER (INPUT icCliType AS CHARACTER ):
    FIND CLIType WHERE CLIType.Clitype = icCliType NO-LOCK NO-ERROR.
    IF AVAILABLE CLIType THEN RETURN CLIType.CliName.
    RETURN "".
END.    