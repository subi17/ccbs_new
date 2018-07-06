/*------------------------------------------------------------------------
    File        : transfer_fees.p
    Purpose     : transfer fees from bFLMobSub to bMLMobSub
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

DEFINE BUFFER bMLMobSub   FOR MobSub. /* Mobile Line Subscription buffer */
DEFINE BUFFER bFLMobSub   FOR MobSub. /* Fixed Line Subscription buffer  */
DEFINE BUFFER bOldMLOwner FOR MSOwner.
DEFINE BUFFER bOldFLOwner FOR MSOwner.
DEFINE BUFFER bSubRequest FOR MsRequest.
DEFINE BUFFER bMLFixedFee FOR FixedFee.
DEFINE BUFFER bFLFixedFee FOR FixedFee.
DEFINE BUFFER MsRequest   FOR MsRequest.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMLMobSub     AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhFLMobSub     AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhOldMLMsOwner AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhOldFLMsOwner AS HANDLE NO-UNDO.
   
   lhMLMobSub = BUFFER bMLMobSub:HANDLE.
   RUN StarEventInitialize(lhMLMobSub).

   lhFLMobSub = BUFFER bFLMobSub:HANDLE.
   RUN StarEventInitialize(lhFLMobSub).

   lhOldMLMsOwner = BUFFER bOldMLOwner:HANDLE.
   RUN StarEventInitialize(lhOldMLMsOwner).

   lhOldFLMsOwner = BUFFER bOldFLOwner:HANDLE.
   RUN StarEventInitialize(lhOldFLMsOwner).

END.

DEF TEMP-TABLE ttContract NO-UNDO
   FIELD DCEvent   AS CHAR
   FIELD PerContID AS INT
   FIELD CreateFee AS LOG
   FIELD ActTS     AS DEC.

DEFINE INPUT PARAMETER iiMsRequestId AS INTEGER NO-UNDO.

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

FUNCTION fgetCliTypeName RETURNS CHARACTER
   (INPUT icCliType AS CHARACTER):
    
   DEFINE BUFFER CLIType FOR CLIType.       
    
   FIND CLIType NO-LOCK WHERE 
        CLIType.Clitype EQ icCliType NO-ERROR.
    
   IF AVAILABLE CLIType THEN 
      RETURN CLIType.CliName.
    
   RETURN "".
END.   

DO ON ERROR UNDO , LEAVE :
    
    FIND MsRequest NO-LOCK WHERE 
         MSRequest.MsRequest EQ iiMsRequestId NO-ERROR.

    IF NOT AVAILABLE MsRequest THEN 
        RETURN "Invalid MsRequest".

    IF fChkSubRequest(iiMsRequestId) AND 
       CAN-FIND(FIRST bSubRequest NO-LOCK WHERE
                      bSubRequest.OrigRequest EQ iiMsRequestId AND 
                      bSubRequest.ReqType     EQ {&REQTYPE_SUBSCRIPTION_TERMINATION}) THEN DO:
        
        FIND ActionLog NO-LOCK WHERE 
             ActionLog.Brand     EQ Syst.Var:gcBrand      AND    
             ActionLog.TableName EQ "MsRequest"           AND 
             ActionLog.KeyValue  EQ STRING(iiMsRequestId) AND 
             ActionLog.ActionID  EQ "FixedFees"           NO-ERROR.

        IF AVAILABLE ActionLog THEN DO TRANSACTION: 
            FIND FixedFee EXCLUSIVE-LOCK WHERE 
                 FixedFee.FFNum EQ INTEGER(ActionLog.ActionChar) NO-ERROR NO-WAIT.
            IF AVAILABLE FixedFee THEN 
                FixedFee.Active = YES.
            RELEASE FixedFee.
        END.  

        fReqStatus({&REQUEST_STATUS_DONE},"").
        
        RETURN. 
    END.
    
    FIND CLIType NO-LOCK WHERE 
         CLIType.ClIType EQ MSRequest.ReqCParam1 NO-ERROR.

    IF NOT AVAILABLE CLIType THEN 
    DO:
        fReqError("Invalid CliType.").
        RETURN.
    END.

    IF CLIType.TariffType EQ {&CLITYPE_TARIFFTYPE_FIXEDONLY} THEN 
    DO: 
        FIND bFLMobSub NO-LOCK WHERE 
             bFLMobSub.MsSeq EQ MSRequest.MsSeq NO-ERROR.

        FIND bMLMobSub NO-LOCK WHERE 
             bMLMobSub.Cli EQ ENTRY(1, MSRequest.reqCparam3 ,"|") NO-ERROR.
    END.
    ELSE IF CLIType.TariffType EQ {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN 
    DO:           
        FIND bMLMobSub NO-LOCK WHERE 
             bMLMobSub.MsSeq EQ MSRequest.MsSeq NO-ERROR.

        FIND bFLMobSub NO-LOCK WHERE 
             bFLMobSub.Cli EQ ENTRY(1, MSRequest.reqCparam3 ,"|") NO-ERROR.
    END.
    ELSE 
    DO:
        fReqError("Invalid TariffType").
        RETURN.
    END.
    
    IF NOT AVAILABLE bMLMobSub OR 
       NOT AVAILABLE bFLMobSub THEN 
    DO:
        IF NOT AVAILABLE bMLMobSub THEN 
            fReqError("Mobile line not found.").
        ELSE 
            fReqError("Fixed line not found.").
        RETURN.
    END.
    
    IF CAN-FIND(FIRST bSubRequest NO-LOCK WHERE 
                      bSubRequest.OrigRequest EQ iiMsRequestId                       AND 
                      bSubRequest.ReqType     EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND 
                      bSubRequest.ReqStatus   EQ {&REQUEST_STATUS_DONE})             THEN DO:
        RUN pCreateTermRequest.
        RETURN.
    END.

    IF CAN-FIND(FIRST bSubRequest NO-LOCK WHERE 
                      bSubRequest.OrigRequest EQ iiMsRequestId                       AND 
                      bSubRequest.ReqType     EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND 
                      bSubRequest.ReqStatus   EQ {&REQUEST_STATUS_HOLD})             THEN 
        DO TRANSACTION ON ERROR UNDO , THROW:
            
            FOR EACH bFLFixedFee NO-LOCK WHERE 
                     bFLFixedFee.Brand     EQ Syst.Var:gcBrand         AND
                     bFLFixedFee.HostTable EQ 'MobSub'                 AND 
                     bFLFixedFee.KeyValue  EQ  STRING(bFLMobSub.MsSeq) AND 
                     bFLFixedFee.BillCode  BEGINS "PAYTERM":
                CREATE bMLFixedFee.
                BUFFER-COPY bFLFixedFee TO bMLFixedFee 
                ASSIGN bMLFixedFee.FFNum       = NEXT-VALUE(Contract)
                       bMLFixedFee.HostTable   = 'MobSub'
                       bMLFixedFee.KeyValue    = STRING(bMLMobSub.MsSeq)
                       bMLFixedFee.SourceTable = 'FixedFee'
                       bMLFixedFee.SourceKey   = STRING(bFLFixedFee.FFNum) 
                       bMLFixedFee.Active      = FALSE.

                fMakeContractMore(bMLFixedFee.FFNum, bMLFixedFee.endPeriod).

                lcFFNums = lcFFNums + "," + STRING(bMLFixedFee.FFNum) .
            END.

            lcFFNums = TRIM(lcFFNums).
    
            CREATE ActionLog.
            ASSIGN ActionLog.Brand        = Syst.Var:gcBrand   
                   ActionLog.TableName    = "MsRequest"  
                   ActionLog.KeyValue     = STRING(iiMsRequestId)
                   ActionLog.UserCode     = 'Newton'
                   ActionLog.ActionID     = "FixedFees"
                   ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
                   ActionLog.ActionChar   = lcFFNums
                   ActionLog.CustNum      = MSRequest.Custnum
                   ActionLog.ActionTS     = Func.Common:mMakeTS().        
            
            FIND CURRENT bMLMobSub EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            FIND CURRENT bFLMobSub EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            
            FIND FIRST bOldMLOwner EXCLUSIVE-LOCK WHERE 
                       bOldMLOwner.MSSEQ  EQ bMLMobSub.MSSeq       AND 
                       bOldMLOwner.CLI    EQ bMLMobSub.CLI         AND 
                       bOldMLOwner.TSEND  >= Func.Common:mMakeTS() NO-ERROR.
            
            FIND FIRST bOldFLOwner EXCLUSIVE-LOCK WHERE 
                       bOldFLOwner.MSSEQ EQ bFLMobSub.MSSeq       AND 
                       bOldFLOwner.CLI   EQ bFLMobSub.CLI         AND 
                       bOldFLOwner.TSEND >= Func.Common:mMakeTS() NO-ERROR.
           
            IF llDoEvent THEN DO:
               RUN StarEventSetOldBuffer(lhOldMLMsOwner).
               RUN StarEventSetOldBuffer(lhOldFLMsOwner).
               RUN StarEventSetOldBuffer(lhMLMobSub).
               RUN StarEventSetOldBuffer(lhFLMobSub).
            END.

            ASSIGN bOldMLOwner.TsEnd     = Func.Common:mMakeTS()
                   bOldFLOwner.TsEnd     = Func.Common:mMakeTS()
                   lcFixedNumber         = bFLMobSub.FixedNumber  
                   bFLMobSub.FixedNumber = ? 
                   bMLMobSub.FixedNumber = lcFixedNumber.

            IF llDoEvent THEN DO:
                RUN StarEventMakeModifyEvent(lhOldMLMsOwner).
                RUN StarEventMakeModifyEvent(lhOldFLMsOwner).
                RUN StarEventMakeModifyEvent(lhMLMobSub).
                RUN StarEventMakeModifyEvent(lhFLMobSub).
            END.

            CREATE MSOwner.
            BUFFER-COPY bOldMLOwner TO Msowner
            ASSIGN MSOwner.TsBegin     = Func.Common:mMakeTS()
                   MSOWner.TSEnd       = 99999999.99999
                   MsOwner.CLIEvent    = "FixedNumber"
                   MSOwner.FixedNumber = bMLMobSub.FixedNumber .
                
            IF llDoEvent THEN 
               fMakeCreateEvent((BUFFER MsOwner:HANDLE),
                                 "",
                                 Syst.Var:katun,
                                 "").                

            CREATE MSOwner.
            BUFFER-COPY bOldFLOwner TO Msowner
            ASSIGN MSOwner.TsBegin     = Func.Common:mMakeTS()
                   MSOWner.TSEnd       = 99999999.99999
                   MsOwner.CLIEvent    = "FixedNumber"
                   MSOwner.FixedNumber = bFLMobSub.FixedNumber.
                
            IF llDoEvent THEN 
               fMakeCreateEvent((BUFFER MsOwner:HANDLE),
                                "",
                                Syst.Var:katun,
                                "").                
            
            FIND Order NO-LOCK WHERE 
                 Order.Brand EQ Syst.Var:gcBrand AND 
                 Order.MSSeq EQ bFLMobSub.MsSeq  NO-ERROR.

            IF NOT AVAILABLE Order THEN
                UNDO, THROW NEW Progress.Lang.AppError 
                               ("Order not found for bFLMobSub",2). 
            
            CREATE ActionLog.
            ASSIGN ActionLog.Brand        = Syst.Var:gcBrand    
                   ActionLog.TableName    = "FixedNumber"  
                   ActionLog.KeyValue     = bMLMobSub.FixedNumber
                   ActionLog.UserCode     = 'Newton'
                   ActionLog.ActionID     = "OrderId"
                   ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
                   ActionLog.ActionChar   = STRING(order.OrderID)
                   ActionLog.CustNum      = MSRequest.Custnum
                   ActionLog.ActionTS     = Func.Common:mMakeTS().   
                                
            RELEASE bMLMobSub.
            RELEASE bFLMobSub.
            RELEASE MSOwner.
            
            FIND bSubRequest EXCLUSIVE-LOCK WHERE 
                 bSubRequest.OrigRequest EQ iiMsRequestId                       AND 
                 bSubRequest.ReqType     EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} NO-ERROR NO-WAIT.
    
            IF NOT AVAILABLE bSubRequest THEN
                UNDO , THROW NEW Progress.Lang.AppError 
                       ( "STC request not found " , 1 ). 
            
            ASSIGN bSubRequest.ReqStatus = {&REQUEST_STATUS_NEW}
                   bSubRequest.Mandatory = 1.
            
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
  
  DEFINE BUFFER bSubRequest FOR MsRequest.
  DEFINE BUFFER crtDCCLi    FOR DCCli. 
  DEFINE BUFFER pstDCCli    FOR DCCli.    
  DEFINE BUFFER Order       FOR Order.

  IF CAN-FIND(FIRST bSubRequest NO-LOCK WHERE 
                    bSubRequest.OrigRequest EQ iiMsRequestId                    AND 
                    bSubRequest.ReqType     EQ {&REQTYPE_CONTRACT_TERMINATION}) THEN DO:

      IF fChkSubRequest(iiMsRequestId) AND 
         NOT CAN-FIND(FIRST bSubRequest NO-LOCK WHERE 
                            bSubRequest.OrigRequest EQ iiMsRequestId                        AND 
                            bSubRequest.ReqType     EQ {&REQTYPE_SUBSCRIPTION_TERMINATION}) THEN DO:
       
          FIND FIRST Order NO-LOCK WHERE
                     Order.Brand EQ Syst.Var:gcBrand AND  
                     Order.MSSeq EQ bFLMobSub.MSSeq  NO-ERROR.

          IF AVAILABLE Order THEN 
             ASSIGN lcMemoString = " &1: &2 (Order ID: &3) + &4: &5 --> &6 (&7 ) "
                    lcMemoString = SUBSTITUTE(lcMemoString,
                                              bFLMobSub.Cli,
                                              fgetCliTypeName(bFLMobSub.CliType),
                                              STRING(Order.OrderId),
                                              bMLMobSub.Cli,
                                              fgetCliTypeName(bMLMobSub.CliType),
                                              fgetCliTypeName(MsRequest.ReqCParam2),
                                              STRING(TODAY,"99-99-9999")).
          ELSE 
             ASSIGN lcMemoString = " &1: &2 + &4: &5 --> &6 (&7 ) "
                    lcMemoString = SUBSTITUTE(lcMemoString,
                                              bFLMobSub.Cli,
                                              fgetCliTypeName(bFLMobSub.CliType),
                                              '',
                                              bMLMobSub.Cli,
                                              fgetCliTypeName(bMLMobSub.CliType),
                                              fgetCliTypeName(MsRequest.ReqCParam2),
                                              STRING(TODAY,"99-99-9999")).
                      
          Func.Common:mWriteMemo("MobSub",
                                 STRING(bFLMobSub.MsSeq),
                                 bFLMobSub.Custnum,
                                 "Tariff Change",
                                 lcMemoString ).

          Func.Common:mWriteMemo("MobSub",
                                 STRING(bMLMobSub.MsSeq),
                                 bMLMobSub.Custnum,
                                 "Tariff Change",
                                 lcMemoString ).
                               
          ASSIGN ldaSecSIMTermDate  = ADD-INTERVAL(TODAY, 1,"months")
                 ldaSecSIMTermDate  = Func.Common:mLastDayOfMonth(ldaSecSIMTermDate)
                 ldeSecSIMTermStamp = Func.Common:mMake2DT(ldaSecSIMTermDate,86399).

          fInitialiseValues({&SUBSCRIPTION_TERM_REASON_MULTISIM},
                            fIsYoigoCLI(bFLMobSub.CLI), 
                            fIsMasmovilCLI(bFLMobSub.CLI),
                            OUTPUT liMsisdnStat,
                            OUTPUT liSimStat,
                            OUTPUT liQuarTime).
            
          liTermRequest = fTerminationRequest(bFLMobSub.Msseq,
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

           FIND MsRequest NO-LOCK WHERE 
                MSRequest.MsRequest EQ iiMsRequestId NO-ERROR.

           IF lcError > "" THEN 
               fReqStatus({&REQUEST_STATUS_REJECTED}, 
                          "Subscription termination failed").
           ELSE DO: 
               
               RUN Mm/deletemobsub.p(liTermRequest).
               
               fReqStatus({&REQUEST_STATUS_SUB_REQUEST_PENDING},"").

           END.    
      END.
      RETURN.
  END.
  
  FOR EACH crtDCCLi EXCLUSIVE-LOCK WHERE 
           crtDCCLi.Brand EQ Syst.var:gcBrand AND 
           crtDCCLi.Msseq EQ bMLMobSub.MsSeq:

     FIND FIRST pstDCCli NO-LOCK WHERE 
                pstDCCli.Brand   EQ crtDCCLi.Brand   AND
                pstDCCli.MSSeq   EQ bFLMobSub.msseq  AND 
                pstDCCli.DCEvent EQ crtDCCLi.DCEvent NO-ERROR.

     IF AVAILABLE pstDCCli THEN   
        crtDCCLi.ValidTo = pstDCCli.ValidTo.

  END.  
    
  FOR EACH DCCLI EXCLUSIVE-LOCK WHERE 
           DCCLI.MsSeq   EQ bFLMobSub.MsSeq AND 
           DCCLI.ValidTo >= TODAY:
         
      FIND FIRST DayCampaign NO-LOCK WHERE 
                 DayCampaign.Brand   EQ Syst.Var:gcBrand AND 
                 DayCampaign.DcEvent EQ DCCLI.DcEvent    NO-ERROR.
      
      DCCLI.TermDate = ?.
      
      IF CAN-FIND(FIRST ttContract NO-LOCK WHERE
                        ttContract.DCEvent EQ DCCLI.DCEvent) THEN NEXT.

      CREATE ttContract.
      ASSIGN ttContract.DCEvent   = DCCLI.DCEvent
             ttContract.CreateFee = DCCLI.CreateFee
             ttContract.PerContID = (IF AVAIL DayCampaign AND
                                              DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT}
                                        THEN DCCLI.PerContractID 
                                     ELSE 0).
   END.    
    
   ldCurrTS = Func.Common:mMakeTS().
   
   FOR EACH MServiceLimit EXCLUSIVE-LOCK WHERE 
            MServiceLimit.MsSeq EQ bFLMobSub.MsSeq AND 
            MServiceLimit.EndTS GT ldCurrTS, 
      FIRST ServiceLimit NO-LOCK USE-INDEX SlSeq WHERE 
            ServiceLimit.SLSeq EQ MServiceLimit.SLSeq:

      FIND FIRST DayCampaign NO-LOCK WHERE 
                 DayCampaign.Brand     EQ Syst.Var:gcBrand       AND 
                 DayCampaign.DCEvent   EQ ServiceLimit.GroupCode AND 
                 DayCampaign.ValidFrom <= TODAY                  AND 
                 DayCampaign.ValidTo   >= TODAY                  NO-ERROR.
              
      IF AVAILABLE DayCampaign THEN DO:
         IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 AND
            STRING(DayCampaign.DCType) <> {&DCTYPE_CUMULATIVE_RATING}
         THEN DO:
            FIND FIRST ttContract NO-LOCK WHERE
                       ttContract.DCEvent EQ ServiceLimit.GroupCode NO-ERROR.
            IF NOT AVAILABLE ttContract THEN DO:
               CREATE ttContract.
               ASSIGN ttContract.DCEvent   = ServiceLimit.GroupCode
                      ttContract.CreateFee = (DayCampaign.TermFeeModel > "").
            END.
         END.
      END.
      ELSE 
         MServiceLimit.EndTS = ldCurrTS.
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

      liTermRequest = fPCActionRequest(bFLMobSub.MsSeq,  
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
   
   IF CAN-FIND(FIRST bSubRequest NO-LOCK WHERE 
                     bSubRequest.OrigRequest EQ iiMsRequestId                    AND 
                     bSubRequest.ReqType     EQ {&REQTYPE_CONTRACT_TERMINATION}) THEN DO:
        FIND MsRequest NO-LOCK WHERE 
             MSRequest.MsRequest EQ iiMsRequestId NO-ERROR.
        fReqStatus({&REQUEST_STATUS_SUB_REQUEST_PENDING}, "").
   END.
      
END PROCEDURE.     

