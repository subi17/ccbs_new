{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/log.i}
{Func/date.i}
{Func/memo.i}
{Func/cparam2.i}
{Func/ftransdir.i}
  
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

    RUN pReadFile.

    RUN pUpdateStatus.

END.

PROCEDURE pUpdateStatus:
    DEF VAR lcLogFile  AS CHAR NO-UNDO.
    DEF VAR lcDateTime AS CHAR NO-UNDO.
    DEF VAR lcMsgType  AS CHAR NO-UNDO.

    ASSIGN 
        lcDateTime = REPLACE(ISO-DATE(TODAY),"-","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","")
        lcLogFile  = fCParamC('ActivationIncomingLogFileName')
        lcLogFile  = REPLACE(lcLogFile,"#DATETIME",lcDateTime).

    OUTPUT TO VALUE(lcLogFile) APPEND.
    FOR EACH ttCustomer,
        FIRST TPService WHERE TPService.SerialNbr = ttCustomer.SerialNbr AND TPService.ServType = "Television" AND TPService.ServStatus = {&STATUS_ONGOING} NO-LOCK:

        /* ASSIGN lcMsgType = (IF ttCustomer.ActionType = "CreateSubscriber" THEN "" ELSE ""). */                            
        FIND FIRST TPServiceMessage WHERE TPServiceMessage.MsSeq         = TPService.MsSeq                    AND 
                                          TPServiceMessage.ServSeq       = TPService.ServSeq                  AND
                                          TPServiceMessage.Source        = {&SOURCE_TMS}                      AND
                                          TPServiceMessage.MessageStatus = {&PENDING_ACTIVATION_CONFIRMATION} AND
                                          TPServiceMessage.MessageType   = {&TYPE_ACTIVATION}                 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF LOCKED TPServiceMessage OR NOT AVAIL TPServiceMessage THEN 
        DO:
            PUT UNFORMATTED "Customer: '" + ttCustomer.CustomerId + "' with serial number: '" + ttCustomer.SerialNbr + "' failed to update" SKIP.
            NEXT.
        END.      

        IF AVAIL TPServiceMessage THEN 
        DO:
            IF ttCustomer.StatusCode = "0" THEN 
            DO:
                ASSIGN
                    TPServiceMessage.UpdateTS       = fMakeTS()
                    TPServiceMessage.ResponseCode   = ttCustomer.StatusCode
                    TPServiceMessage.AdditionalInfo = ttCustomer.Description 
                    TPServiceMessage.MessageStatus  = {&STATUS_HANDLED}.
            END.
            ELSE 
            DO:
                ASSIGN
                    TPServiceMessage.UpdateTS       = fMakeTS()
                    TPServiceMessage.ResponseCode   = ttCustomer.StatusCode
                    TPServiceMessage.AdditionalInfo = ttCustomer.Description 
                    TPServiceMessage.MessageStatus  = {&STATUS_ERROR}.
            END.
        END.

        BUFFER TPService:FIND-CURRENT(EXCLUSIVE-LOCK, NO-WAIT).
        IF AVAIL TPService THEN 
            ASSIGN 
                TPService.UpdateTS   = TPServiceMessage.UpdateTS
                TPService.ServStatus = {&STATUS_ACTIVATED}.  
    END.
    OUTPUT CLOSE.

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
        lcIncomingDir    = fCParamC('ActivationIncomingDir')
        lcIncomingArcDir = fCParamC('ActivationIncomingArcDir').

    INPUT FROM OS-DIR(lcIncomingDir).
    REPEAT:
        IMPORT ^ lcFlag lcFileName. 
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
            ttCustomer.StatusCode  = ENTRY(10,lcData)
            ttCustomer.Description = ENTRY(11,lcData).

    END.
    INPUT CLOSE.
    
END PROCEDURE.

FINALLY:
   EMPTY TEMP-TABLE ttCustomer.
END.


