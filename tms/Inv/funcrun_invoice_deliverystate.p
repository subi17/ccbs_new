/* ----------------------------------------------------------------------
  MODULE .......: funcrun_invoice_deliverystate.p
  TASK .........: Mark Invoice Delivery State to 1 (function execution)
  APPLICATION ..: TMS
  AUTHOR .......: Vikas 
  CREATED ......: 19.07.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "Cron".

{Syst/funcrunprocess_run.i}

DEF VAR liFRProcessID    AS INT  NO-UNDO.
DEF VAR liFRExecID       AS INT  NO-UNDO.
DEF VAR lcRunMode        AS CHAR NO-UNDO.
DEF VAR liUpdateInterval AS INT  NO-UNDO.
DEF VAR liMarked         AS INT  NO-UNDO.
DEF VAR liInvType        AS INT  NO-UNDO.
DEF VAR liDeliveryState  AS INT  NO-UNDO.
DEF VAR ldInvDate        AS DATE NO-UNDO.

/****** Main start ********/

RUN pInitializeFuncRunProcess(OUTPUT liFRProcessID,
                              OUTPUT liFRExecID,
                              OUTPUT lcRunMode,
                              OUTPUT liUpdateInterval).
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pGetFuncRunProcessParameters(liFRProcessID).

liInvType = fSetFuncRunIntParameter(1).
ldInvDate = fSetFuncRunDateParameter(2).
liDeliveryState = fSetFuncRunIntParameter(3).

RUN invoice_deliverystate.p(INPUT ldInvDate,
                            INPUT liInvType,
                            INPUT liDeliveryState,
                            INPUT liFRProcessID,
                            INPUT liUpdateInterval,
                            OUTPUT liMarked).

IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   RUN pCancelFuncRunProcess(liFRProcessID,RETURN-VALUE).
   QUIT.
END.

RUN pFinalizeFuncRunProcess(liFRProcessID,liMarked).

QUIT.

/******** Main end *******/
