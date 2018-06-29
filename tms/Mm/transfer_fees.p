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

DEFINE BUFFER mobileLine FOR MobSub.
DEFINE BUFFER fixedLine  FOR MobSub.
DEFINE BUFFER oldMLOwner FOR MSOwner.
DEFINE BUFFER oldFLOwner FOR MSOwner.
DEFINE BUFFER subReq     FOR MsRequest.
DEFINE BUFFER mlFFee     FOR fixedFee.
DEFINE BUFFER flFFee     FOR fixedFee.


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhmobileLine AS HANDLE NO-UNDO.
   lhmobileLine = BUFFER mobileLine:HANDLE.
   RUN StarEventInitialize(lhmobileLine).

   DEFINE VARIABLE lhfixedLine AS HANDLE NO-UNDO.
   lhfixedLine = BUFFER fixedLine:HANDLE.
   RUN StarEventInitialize(lhfixedLine).

   DEFINE VARIABLE lhMLOwner AS HANDLE NO-UNDO.
   lhMLOwner = BUFFER oldMLOwner:HANDLE.
   RUN StarEventInitialize(lhMLOwner).

   DEFINE VARIABLE lhFLOwner AS HANDLE NO-UNDO.
   lhFLOwner = BUFFER oldFLOwner:HANDLE.
   RUN StarEventInitialize(lhFLOwner).
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

DO ON ERROR UNDO , LEAVE :
    
    FIND MsRequest WHERE MSRequest.MsRequest = iiMsRequestId NO-LOCK NO-ERROR.
    IF NOT AVAILABLE MsRequest THEN 
        RETURN "Invalid MsRequest".
    
    
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
                flFFee.Brand = MSRequest.Brand AND
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
                ActionLog.Brand        = MSRequest.Brand  
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
                 WHERE oldMLOwner.MSSEQ  = Mobsub.MSSeq  
                   AND oldMLOwner.CLI    = Mobsub.CLI  
                   AND oldMLOwner.TSEND >= Func.Common:mMakeTS()  NO-ERROR.
            
            FIND FIRST oldFLOwner EXCLUSIVE-LOCK 
                 WHERE oldFLOwner.MSSEQ  = Mobsub.MSSeq  
                   AND oldFLOwner.CLI    = Mobsub.CLI  
                   AND oldFLOwner.TSEND >= Func.Common:mMakeTS()  NO-ERROR.
            
            IF llDoEvent THEN DO: 
                RUN StarEventSetOldBuffer(lhMLOwner).
                RUN StarEventSetOldBuffer(lhFLOwner).
            END.

            ASSIGN 
                oldMLOwner.TsEnd       = Func.Common:mMakeTS()
                oldFLOwner.TsEnd       = Func.Common:mMakeTS().

            IF llDoEvent THEN DO: 
                RUN StarEventMakeModifyEvent(lhMLOwner).
                RUN StarEventMakeModifyEvent(lhFLOwner).
                
                RUN StarEventSetOldBuffer(lhmobileLine).
                RUN StarEventSetOldBuffer(lhfixedLine).
            END.

            ASSIGN 
                mobileLine.FixedNumber = fixedline.FixedNumber
                fixedLine.FixedNumber  = ? .

            IF llDoEvent THEN DO: 
                RUN StarEventMakeModifyEvent(lhmobileLine).
                RUN StarEventMakeModifyEvent(lhfixedLine).
            END.

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
                 Order.Brand = fixedline.Brand AND 
                 Order.MSSeq = fixedline.MsSeq NO-LOCK NO-ERROR.

            IF NOT AVAILABLE order THEN
                UNDO, THROW NEW Progress.Lang.AppError 
                               ( "Order not found for fixedline" , 2 ). 
            
            CREATE ActionLog.
            ASSIGN 
                ActionLog.Brand        = fixedline.Brand   
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

PROCEDURE pCreateTermRequest:
    
  DEFINE VARIABLE ldCurrTS AS DECIMAL NO-UNDO.
  DEFINE VARIABLE llCreateFee AS LOGICAL NO-UNDO.
  DEFINE VARIABLE liTermRequest AS INTEGER NO-UNDO.
  DEFINE VARIABLE lcError AS CHARACTER NO-UNDO.
  DEFINE BUFFER subReq FOR MSRequest.
  
  IF CAN-FIND(FIRST subreq WHERE SubReq.OrigRequest = iiMsRequestId 
                AND subreq.ReqType = {&REQTYPE_CONTRACT_TERMINATION} ) THEN DO:
      IF fChkSubRequest(iiMsRequestId) THEN DO:
          FIND ActionLog WHERE 
               ActionLog.Brand        = MSRequest.Brand        AND    
               ActionLog.TableName    = "MsRequest"            AND 
               ActionLog.KeyValue     = STRING(iiMsRequestId)  AND 
               ActionLog.ActionID     = "FixedFees" NO-LOCK NO-ERROR.
          IF AVAILABLE ActionLog THEN DO: 
            FIND FixedFee WHERE 
                 FixedFee.FFNum = INTEGER(ActionLog.ActionChar) 
                 EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE FixedFee THEN 
                FixedFee.Active = YES.
            RELEASE FixedFee.
          END.  
           fReqStatus( {&REQUEST_STATUS_DONE},""). 
      END.
      RETURN.
  END.
    
  FOR EACH DCCLI EXCLUSIVE-LOCK 
     WHERE DCCLI.MsSeq = fixedline.MsSeq  
       AND DCCLI.ValidTo >= TODAY:
         
      FIND FIRST DayCampaign NO-LOCK 
           WHERE DayCampaign.Brand = fixedline.brand 
             AND DayCampaign.DcEvent = DCCLI.DcEvent NO-ERROR.
      
      DCCLI.TermDate = ?.

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
           WHERE DayCampaign.Brand      = fixedline.Brand 
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
                 DayCampaign.Brand      = fixedline.Brand    AND 
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
                          {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
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