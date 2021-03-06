{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/log.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/fmakemsreq.i}
{Func/orderfunc.i}
{Mc/dpmember.i}

DEFINE TEMP-TABLE ttCancellation
    FIELD Voucher    AS CHAR
    FIELD CancelDate AS DATE
    FIELD FileName   AS CHAR
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
    FOR EACH ttCancellation,
        EACH TPService WHERE TPService.SkyTvVoucher = ttCancellation.Voucher AND    
                             TPService.ServType     = "Television"           AND    
                             TPService.ServStatus   = {&STATUS_HANDLED}      AND 
                             TPService.Operation    = {&TYPE_ACTIVATION}     EXCLUSIVE-LOCK:

        ASSIGN 
            TPService.VoucherStatus   = "Cancelled"
            TPService.VoucherCancelDt = ttCancellation.CancelDate.

        FIND FIRST MobSub WHERE MobSub.MsSeq = TPService.MsSeq NO-LOCK NO-ERROR.
        IF AVAIL MobSub THEN
        DO:
            FIND FIRST Customer WHERE Customer.CustNum = MobSub.AgrCust NO-LOCK NO-ERROR.
            IF AVAIL Customer THEN
                ASSIGN lcDiscPlan = fGetRegionDiscountPlan(Customer.Region).    
        END.

        IF lcDiscPlan > ""
        THEN fCloseDiscount(lcDiscPlan,
                            TPService.MsSeq,
                            TPService.VoucherCancelDt,
                            NO).
    END.

    RETURN "".

    FINALLY:
       OUTPUT CLOSE.
       RELEASE TPService.
       RELEASE DPMember.
       IF llDoEvent
       THEN fCleanEventObjects().
    END FINALLY.

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
        lcIncomingDir    = fCParamC('VoucherCancelationIncomingDir')
        lcIncomingArcDir = fCParamC('VoucherCancelationIncomingArcDir').

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

        CREATE ttCancellation.
        ASSIGN
            ttCancellation.FileName   = icFileName
            ttCancellation.Voucher    = ENTRY(1,lcData)
            ttCancellation.CancelDate = DATE(ENTRY(1,ENTRY(2,lcData)," ")).

    END.
    INPUT CLOSE.
    
END PROCEDURE.

FINALLY:
   EMPTY TEMP-TABLE ttCancellation.
END.


