{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/log.i}
{Func/memo.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/fmakemsreq.i}
{Func/orderfunc.i}

DEFINE TEMP-TABLE ttRedemption
    FIELD Voucher       AS CHAR
    FIELD ExpiryDate    AS DATE
    FIELD VoucherStatus AS CHAR 
    FIELD TimesUsed     AS INTE
    FIELD RedemDate     AS DATE
    FIELD FileName      AS CHAR
    INDEX IdxVoucher IS UNIQUE PRIMARY Voucher.

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
        ldeActStamp = Func.Common:mMakeTS()    
        lcDateTime  = REPLACE(ISO-DATE(TODAY),"-","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","")
        lcLogFile   = fCParamC('ActivationIncomingLogFileName')
        lcLogFile   = REPLACE(lcLogFile,"#DATETIME",lcDateTime).

    OUTPUT TO VALUE(lcLogFile) APPEND.
    FOR EACH ttRedemption,
        EACH TPService WHERE TPService.SkyTvVoucher = ttRedemption.Voucher AND    
                             TPService.ServType     = "Television"         AND    
                             TPService.ServStatus   = {&STATUS_HANDLED}    AND 
                             TPService.Operation    = {&TYPE_ACTIVATION}   EXCLUSIVE-LOCK:

        ASSIGN 
            TPService.VoucherStatus   = ttRedemption.VoucherStatus
            TPService.VoucherExpiryDt = ttRedemption.ExpiryDate
            TPService.VoucherRedemDt  = ttRedemption.RedemDate
            TPService.RedemFile       = ttRedemption.FileName.    
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
        lcIncomingDir    = fCParamC('VoucherRedemptionIncomingDir')
        lcIncomingArcDir = fCParamC('VoucherRedemptionIncomingArcDir').

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

        CREATE ttRedemption.
        ASSIGN
            ttRedemption.FileName      = icFileName
            ttRedemption.Voucher       = ENTRY(1,lcData)
            ttRedemption.ExpiryDate    = DATE(ENTRY(1,ENTRY(2,lcData)," "))    
            ttRedemption.VoucherStatus = ENTRY(3,lcData)    
            ttRedemption.TimesUsed     = INT(ENTRY(4,lcData))
            ttRedemption.RedemDate     = (IF ENTRY(5,lcData) = "" THEN 
                                             ? 
                                         ELSE 
                                             DATE(ENTRY(1,ENTRY(5,lcData)," "))
                                         ).
    END.
    INPUT CLOSE.
    
END PROCEDURE.

FINALLY:
   EMPTY TEMP-TABLE ttRedemption.
END.


