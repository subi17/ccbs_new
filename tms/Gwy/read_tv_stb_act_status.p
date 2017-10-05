{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/log.i}
{Func/date.i}
{Func/memo.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/fmakemsreq.i}
{Func/orderfunc.i}
{Mc/dpmember.i}

DEF VAR lcCustomerId AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttCustomer
    FIELD CustomerId  AS CHAR
    FIELD CustName    AS CHAR
    FIELD Action      AS CHAR
    FIELD ActionType  AS CHAR
    FIELD Email       AS CHAR
    FIELD Region      AS CHAR
    FIELD Product     AS CHAR
    FIELD SerialNbr   AS CHAR
    FIELD StatusCode  AS CHAR
    FIELD Description AS CHAR
    FIELD FileName    AS CHAR
    INDEX IdxCustomerId IS UNIQUE PRIMARY CustomerId Email.

DEFINE BUFFER AgreeCustomer FOR OrderCustomer.

DO ON ERROR UNDO, THROW:

    RUN pReadIncomingDirectory.

    RUN pUpdateStatus.

END.

PROCEDURE pUpdateStatus:
    DEF VAR lcLogFile   AS CHAR NO-UNDO.
    DEF VAR lcDateTime  AS CHAR NO-UNDO.
    DEF VAR lcMsgType   AS CHAR NO-UNDO.
    DEF VAR lcError     AS CHAR NO-UNDO.
    DEF VAR liRequest   AS INTE NO-UNDO.
    DEF VAR liDiscReq   AS INTE NO-UNDO.
    DEF VAR ldeActStamp AS DECI NO-UNDO.
    DEF VAR lcDiscPlan  AS CHAR NO-UNDO.
    DEF VAR ldeDiscAmt  AS DECI NO-UNDO.
    DEF VAR lcErrMsg    AS CHAR NO-UNDO.
    
    ASSIGN 
        ldeActStamp = fMakeTS()    
        lcDateTime  = REPLACE(ISO-DATE(TODAY),"-","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","")
        lcLogFile   = fCParamC('ActivationIncomingLogFileName')
        lcLogFile   = REPLACE(lcLogFile,"#DATETIME",lcDateTime).

    OUTPUT TO VALUE(lcLogFile) APPEND.
    FOR EACH ttCustomer,
        FIRST TPService WHERE TPService.SerialNbr  = ttCustomer.SerialNbr                       AND    
                              TPService.ServType   = "Television"                               AND    
                              TPService.ServStatus = {&WAITING_FOR_STB_ACTIVATION_CONFIRMATION} AND 
                              TPService.Operation  = {&TYPE_ACTIVATION}                         EXCLUSIVE-LOCK:      

        IF ttCustomer.StatusCode = "0" THEN 
        DO:
            liRequest = fPCActionRequest(TPService.MsSeq,
                                         TPService.Product,
                                         "act",
                                         ldeActStamp,
                                         TRUE,
                                         {&REQUEST_SOURCE_TV_SERVICE_ACTIVATION},
                                         "",
                                         0,
                                         FALSE,
                                         "",
                                         0,
                                         0,
                                         "",
                                         OUTPUT lcError).
            IF liRequest > 0 THEN 
            DO:
                fCreateTPServiceMessage(TPService.MsSeq, TPService.ServSeq, {&SOURCE_TV_STB_VENDOR}, {&STATUS_HANDLED}).
                /*
                IF TPService.Offer > "" THEN 
                    RUN Mc/offeritem_exec.p(TPService.MsSeq,
                                            Order.OrderID,
                                            ldeActStamp,
                                            MsRequest.MsRequest,
                                            {&REQUEST_SOURCE_TV_SERVICE_ACTIVATION}).
                */
                IF LOOKUP(TPService.Product,"AGILESKYTV") > 0 THEN 
                DO:
                    ASSIGN lcDiscPlan = fGetRegionDiscountPlan(ttCustomer.Region).

                    IF lcDiscPlan > "" THEN 
                    DO:
                        FIND FIRST DiscountPlan WHERE DiscountPlan.Brand = gcBrand AND DiscountPlan.DPRuleID = lcDiscPlan NO-LOCK NO-ERROR.
                        IF AVAIL DiscountPlan THEN 
                        DO:
                            FIND FIRST DPRate WHERE DPRate.DPId = DiscountPlan.DPId AND DPRate.ValidFrom <= TODAY AND DPRate.ValidTo >= TODAY NO-LOCK NO-ERROR.
                            IF AVAIL DPRate THEN    
                                ASSIGN ldeDiscAmt = DPRate.DiscValue.
                        END.
                        
                        IF ldeDiscAmt > 0 THEN     
                            ASSIGN liDiscReq = fAddDiscountPlanMember(TPService.MsSeq,
                                                                      lcDiscPlan,
                                                                      ldeDiscAmt,
                                                                      TODAY,
                                                                      0,
                                                                      0,
                                                                      OUTPUT lcErrMsg).

                        IF liDiscReq NE 0 THEN 
                            PUT UNFORMATTED "Customer: '" + ttCustomer.CustomerId + "' with serial number: '" + 
                                                    ttCustomer.SerialNbr + "' failed to activate service: '" + 
                                                    TPService.Product + "' Error: '" + lcErrMsg + "'" SKIP.
                    END.
                END.
            END.    
            ELSE 
            DO:
                fCreateTPServiceMessage(TPService.MsSeq, TPService.ServSeq, {&SOURCE_TMS}, {&STATUS_ERROR}).

                PUT UNFORMATTED "Customer: '" + ttCustomer.CustomerId + "' with serial number: '" + 
                                                ttCustomer.SerialNbr + "' failed to activate service: '" + 
                                                TPService.Product + "' Error: '" + lcError + "'" SKIP.                
            END.        
        END.
        ELSE 
            fCreateTPServiceMessage(TPService.MsSeq, TPService.ServSeq, {&SOURCE_TV_STB_VENDOR}, {&STATUS_ERROR}).

        ASSIGN 
            TPService.ResponseCode   = ttCustomer.StatusCode
            TPService.AdditionalInfo = ttCustomer.Description.    
    END.
    OUTPUT CLOSE.
    RELEASE TPService.
    
    RETURN "".

END PROCEDURE.

PROCEDURE pReadIncomingDirectory:
    DEF VAR liCnt            AS INTE NO-UNDO.
    DEF VAR lcSep            AS CHAR NO-UNDO INIT ",". 
    DEF VAR lcDateTime       AS CHAR NO-UNDO.
    DEF VAR lcIncomingDir    AS CHAR NO-UNDO.
    DEF VAR lcFlag           AS CHAR NO-UNDO.
    DEF VAR lcFileName       AS CHAR NO-UNDO.
    DEF VAR lcIncomingArcDir AS CHAR NO-UNDO.

    ASSIGN 
        lcIncomingDir    = fCParamC('ActConfirmationIncomingDir')
        lcIncomingArcDir = fCParamC('ActConfirmationIncomingArcDir').

    INPUT FROM OS-DIR(lcIncomingDir).
    REPEAT:
        IMPORT ^ lcFileName lcFlag. 
        IF lcFlag <> "F" THEN 
            NEXT.

        RUN pReadFile(lcFileName).   

        fMove2TransDir(lcFileName, "", lcIncomingArcDir).
    END.
    INPUT CLOSE.

    RETURN "".

END PROCEDURE.

PROCEDURE pReadFile:
    DEFINE INPUT PARAMETER icFileName AS CHAR NO-UNDO.

    DEF VAR lcData AS CHAR NO-UNDO.
    
    INPUT FROM VALUE(icFileName).
    REPEAT:
        IMPORT UNFORMATTED lcData.

        IF lcData = "" THEN 
            NEXT.

        CREATE ttCustomer.
        ASSIGN
            ttCustomer.FileName    = icFileName
            ttCustomer.CustomerId  = ENTRY(3,lcData)
            ttCustomer.Action      = ENTRY(1,lcData)    
            ttCustomer.ActionType  = ENTRY(2,lcData)    
            ttCustomer.CustName    = ENTRY(4,lcData)
            ttCustomer.Region      = ENTRY(5,lcData)
            ttCustomer.Email       = ENTRY(6,lcData)
            ttCustomer.Product     = ENTRY(7,lcData)
            ttCustomer.SerialNbr   = ENTRY(8,lcData)
            ttCustomer.StatusCode  = ENTRY(11,lcData)
            ttCustomer.Description = ENTRY(12,lcData).

    END.
    INPUT CLOSE.
    
END PROCEDURE.

FINALLY:
   EMPTY TEMP-TABLE ttCustomer.
END.


