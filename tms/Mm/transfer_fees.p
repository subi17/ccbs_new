/*------------------------------------------------------------------------
    File        : transfer_fees.p
    Purpose     : transfer fees from FixedLine to MobileLine
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : Mon Jun 11 12:10:04 IST 2018
    Notes       : as part of 2p3pMerge
  ----------------------------------------------------------------------*/
  
{Syst/tmsconst.i} 
{Func/msreqfunc.i}
{Func/fmakemsreq.i}
{Func/fsubstermreq.i}
{Func/nncoit2.i }
{Func/msisdn_prefix.i}

DEFINE INPUT PARAMETER iiMsRequestId AS INTEGER   NO-UNDO.

DEFINE BUFFER mobileLine FOR MobSub.
DEFINE BUFFER fixedLine  FOR MobSub.
DEFINE BUFFER subReq     FOR MsRequest.
DEFINE BUFFER mlFFee     FOR fixedFee.
DEFINE BUFFER flFFee     FOR fixedFee.

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

DO ON ERROR UNDO , LEAVE :
    
    FIND MsRequest WHERE MSRequest.MsRequest = iiMsRequestId NO-LOCK NO-ERROR.
    IF NOT AVAILABLE MsRequest THEN 
        RETURN "Invalid MsRequest".
    IF CAN-FIND(FIRST subReq WHERE SubReq.OrigRequest = iiMsRequestId AND 
        SubReq.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION}) THEN 
    DO:
        FIND FIRST subReq 
            WHERE SubReq.OrigRequest = iiMsRequestId AND 
            SubReq.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} NO-LOCK NO-ERROR.
        IF subReq.reqStatus = {&REQUEST_STATUS_DONE} THEN 
            RUN pCreateSTCRequest.
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
        FIND fixedLine WHERE fixedLine.MsSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.
        FIND mobileLine WHERE mobileLine.Cli = ENTRY(1, MSRequest.reqCparam3 ,"|") NO-LOCK NO-ERROR.
    END.
    ELSE IF CLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN 
        DO:           
            FIND mobileLine WHERE mobileLine.MsSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.
            FIND fixedLine WHERE fixedLine.Cli = ENTRY(1, MSRequest.reqCparam3 ,"|") NO-LOCK NO-ERROR.
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
    
    DO TRANSACTION ON ERROR UNDO , THROW:
        
        FOR EACH flFFee NO-LOCK WHERE 
            flFFee.Brand = "1" AND
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
                mlFFee.SourceKey   = STRING(flFFee.FFNum) .
            fMakeContractMore(mlFFee.FFNum  , mlFFee.endPeriod) .
        END.
        
        ASSIGN 
            lcTenant         = BUFFER-TENANT-NAME(fixedLine)
            llYoigoCLI       = fIsYoigoCLI(fixedLine.CLI)
            llMasmovilCLI    = fIsMasmovilCLI(fixedLine.CLI)
            llYoigoTenant    = (IF lcTenant = {&TENANT_YOIGO}    THEN TRUE ELSE FALSE)  
            llMasmovilTenant = (IF lcTenant = {&TENANT_MASMOVIL} THEN TRUE ELSE FALSE).
        
        
        fInitialiseValues(
            INPUT 3 ,
            INPUT llYoigoCLi,
            INPUT llMasmovilCLI,
            OUTPUT piMsisdnStat,
            OUTPUT piSimStat,
            OUTPUT piQuarTime).

        liTerminate = fTerminationRequest( fixedLine.MsSeq,
            0,    
            piMsisdnStat,
            piSimStat,
            piQuarTime,  
            0 ,
            'Merge',
            '',
            {&REQUEST_SOURCE_MERGE_TRANSFER_FEES},
            "",
            MSRequest.MsRequest,
            {&TERMINATION_TYPE_FULL},
            OUTPUT lcError).
    
        IF liTerminate = 0 OR liTerminate = ? THEN 
            UNDO , THROW NEW Progress.Lang.AppError ( "Termination request creation failed." + lcError , 1 ).
        
        CATCH err AS Progress.Lang.Error :
            fReqError("Error occured : " + err:GetMessage(1)).
        END CATCH.  
    END.
END.

PROCEDURE pCreateSTCRequest:
    DEFINE VARIABLE liRequest AS INTEGER NO-UNDO.
    DEFINE VARIABLE  lcInfo   AS CHARACTER NO-UNDO.
    FIND CLIType WHERE CLIType.Clitype = MSRequest.ReqCParam1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CLIType THEN 
    DO:
        fReqError("Invalid CliType.").
        RETURN.
    END.
    IF CLIType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY} THEN 
    DO: 
        FIND mobileLine WHERE mobileLine.Cli = ENTRY(1, MSRequest.reqCparam3 ,"|") NO-LOCK NO-ERROR.
    END.
    ELSE IF CLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN 
    DO:           
        FIND mobileLine WHERE mobileLine.MsSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.
    END.
    ELSE 
    DO:
        fReqError("Invalid TariffType").
        RETURN.
    END.
    IF NOT AVAILABLE mobileLine THEN 
    DO:
        fReqError("Mobile line not found.").
        RETURN.
    END.
    
    IF mobileLine.FixedNumber > ''  AND  mobileLine.FixedNumber NE ? THEN DO: 
        IF NUM-ENTRIES(MSRequest.reqCparam3 ,"|") >  10 THEN 
            liRequest = fCTChangeRequest(mobileLine.MsSeq ,
                              MSRequest.ReqCParam2,
                              ENTRY(3, MSRequest.reqCparam3 ,"|"),
                              ENTRY(4, MSRequest.reqCparam3 ,"|"),
                              DECIMAL(ENTRY(5, MSRequest.reqCparam3 ,"|")),
                              INTEGER(ENTRY(6, MSRequest.reqCparam3 ,"|")) ,
                              INTEGER(ENTRY(7, MSRequest.reqCparam3 ,"|")) ,
                              "" ,
                              LOGICAL(ENTRY(8, MSRequest.reqCparam3 ,"|")) ,
                              LOGICAL(ENTRY(9, MSRequest.reqCparam3 ,"|")) ,
                              "",
                              DECIMAL(ENTRY(10, MSRequest.reqCparam3 ,"|")),
                              {&REQUEST_SOURCE_MERGE_TRANSFER_FEES},
                              0 , /* order id */
                              MSRequest.MsRequest,
                              ENTRY(11, MSRequest.reqCparam3 ,"|"),
                              OUTPUT lcInfo).
         IF liRequest  = 0 OR lcInfo > '' THEN 
            fReqStatus(3,'STCREQ failed ' + lcInfo ).
         ELSE 
            fReqStatus(7,'').
        RETURN.
    END.  
    
    FIND CURRENT mobileLine EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    ASSIGN 
        mobileLine.FixedNumber = ENTRY(2, MSRequest.reqCparam3 ,"|") WHEN NUM-ENTRIES(MSRequest.reqCparam3 ,"|") > 1 .

END PROCEDURE.     