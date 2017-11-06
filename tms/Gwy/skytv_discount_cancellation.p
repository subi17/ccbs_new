{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/log.i}
{Func/memo.i}
{Func/cparam2.i}
{Mc/orderfusion.i}
{Func/orderfunc.i}

DEF VAR liLoop      AS INTE NO-UNDO.

DO ON ERROR UNDO, THROW:

    RUN pProcessRequests.

END.

PROCEDURE pProcessRequests:
    DEF VAR ldtValidate AS DATE NO-UNDO.
    DEF VAR lcDiscPlan  AS CHAR NO-UNDO.

    MESSAGE_LOOP:
    FOR EACH TPService WHERE TPService.VoucherStatus = "Unlocked"         AND 
                             TPService.ServType      = "Television"       AND    
                             TPService.Operation     = {&TYPE_ACTIVATION} AND 
                             TPService.ServStatus    = {&STATUS_HANDLED}  EXCLUSIVE-LOCK:

        ASSIGN ldtValidate = ADD-INTERVAL(TPService.VoucherActiveDt, 5, "weeks").

        IF ldtValidate >= TODAY THEN
            NEXT.

        FIND FIRST MobSub WHERE MobSub.MsSeq = TPService.MsSeq NO-LOCK NO-ERROR.
        IF AVAIL MobSub THEN
        DO:
            FIND FIRST Customer WHERE Customer.CustNum = MobSub.AgrCust NO-LOCK NO-ERROR.
            IF AVAIL Customer THEN
                ASSIGN lcDiscPlan = fGetRegionDiscountPlan(Customer.Region).    
        END.

        IF lcDiscPlan > "" THEN 
        DO:
            FIND FIRST DiscountPlan WHERE DiscountPlan.Brand = Syst.Var:gcBrand AND DiscountPlan.DPRuleID = lcDiscPlan NO-LOCK NO-ERROR.
            IF AVAIL DiscountPlan THEN 
            DO:
                FIND FIRST DPMember WHERE DPMember.DPId      = DiscountPlan.DPId       AND 
                                          DPMember.HostTable = "MobSub"                AND
                                          DPMember.KeyValue  = STRING(TPService.MsSeq) AND
                                          DPMember.ValidTo  >= TODAY                   EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                IF AVAIL DPMember THEN 
                    ASSIGN DPMember.ValidTo = TODAY.
            END.
        END.

        ASSIGN 
            TPService.VoucherStatus   = "Cancelled"
            TPService.VoucherCancelDt = TODAY.

    END.

    RETURN "".

END PROCEDURE.

FINALLY:
   
END.


