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
{Func/nncoit2.i }

DEFINE INPUT PARAMETER iiMsRequestId AS INTEGER   NO-UNDO.

DEFINE BUFFER mobileLine FOR MobSub.
DEFINE BUFFER fixedLine  FOR MobSub.
DEFINE BUFFER subReq     FOR MsRequest.
DEFINE BUFFER mlFFee     FOR fixedFee.
DEFINE BUFFER flFFee     FOR fixedFee.

DEFINE VARIABLE liTerminate AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO.

DO ON ERROR UNDO , LEAVE :
    
    FIND MsRequest WHERE MSRequest.MsRequest = iiMsRequestId NO-LOCK NO-ERROR.
    IF NOT AVAILABLE MsRequest THEN 
        RETURN "Invalid MsRequest".
    IF CAN-FIND(FIRST subReq WHERE SubReq.OrigRequest = iiMsRequestId AND 
                                   SubReq.ReqType = {&REQTYPE_CONTRACT_TERMINATION}) THEN 
        RETURN. /* Nothing to be done here. */
    FIND CLIType WHERE CLIType.Clitype = MSRequest.ReqCParam1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CLIType THEN 
    DO:
        fReqError("Invalid CliType.").
        RETURN.
    END.
    IF CLIType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY} THEN DO: 
        FIND fixedLine WHERE fixedLine.MsSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.
        FIND mobileLine WHERE mobileLine.Cli = ENTRY(1,MSRequest.reqCparam3 ,"|") NO-LOCK NO-ERROR.
    END.
    ELSE IF CLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN DO:           
        FIND mobileLine WHERE mobileLine.MsSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.
        FIND fixedLine WHERE fixedLine.Cli = ENTRY(1,MSRequest.reqCparam3 ,"|") NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        fReqError("Invalid TariffType").
        RETURN.
    END.
    
    IF NOT AVAILABLE mobileLine OR NOT AVAILABLE fixedLine THEN DO:
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
                 flFFee.KeyValue = STRING(mobileLine.MsSeq) AND 
                 flFFee.BillCode BEGINS "PAYTERM" :
            CREATE mlFFee.
            BUFFER-COPY flFFee TO mlFFee 
            ASSIGN 
                mlFFee.FFNum       = NEXT-VALUE(Contract)
                mlFFee.HostTable   = 'mobsub'
                mlFFee.KeyValue    = STRING(fixedLine.MsSeq)
                mlFFee.SourceTable = 'FixedFee'
                mlFFee.SourceKey   = STRING(flFFee.FFNum) .
            fMakeContractMore(mlFFee.FFNum  , mlFFee.endPeriod) .
        END.
        
        liTerminate = fPCActionRequest(Mobileline.MsSeq,
                                       MobileLine.CliType,
                                       "term",
                                       0,
                                       FALSE,   /* create fee */
                                       {&REQUEST_SOURCE_MERGE_TRANSFER_FEES},
                                       "",
                                       MsRequest.MsRequest,
                                       TRUE,
                                       "",
                                       0,
                                       0,
                                       "",
                                       OUTPUT lcError).
    
       IF liTerminate = 0 OR liTerminate = ? THEN 
           UNDO , THROW NEW Progress.Lang.AppError ("Termination request creation failed." + lcError).
        CATCH err AS Progress.Lang.Error :
            fReqError("Error occured : " + err:GetMessage(1)).
        END CATCH.  
    END.
END.

 