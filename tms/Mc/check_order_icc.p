/* ----------------------------------------------------------------------
  MODULE .......: check_order_icc.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: 17.01.18
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".

{Syst/tmsconst.i}
{Func/orderfunc.i}
{Func/fixedlinefunc.i}

DEF VAR lcToday      AS CHAR   NO-UNDO. 
DEF VAR lcLogFile    AS CHAR   NO-UNDO. 
DEF VAR lcOutDir     AS CHAR   NO-UNDO.
DEF VAR ldeCurrentTS AS DEC    NO-UNDO.
DEF VAR ldeCerrada   AS DEC    NO-UNDO.
DEF VAR ldeCurrent   AS DEC    NO-UNDO.
DEF VAR lhOrder      AS HANDLE NO-UNDO.
DEF VAR ldeCerradaTS AS DEC    NO-UNDO.

DEF STREAM strout. 

ASSIGN ldeCurrentTS = Func.Common:mMakeTS()
       lcOutDir     = fCParam("Order","OrderICCValueFolder")
       lcToday      = STRING(YEAR(TODAY),"9999") + 
                      STRING(MONTH(TODAY),"99")  +
                      STRING(DAY(TODAY),"99")    + "_" + 
                      REPLACE(STRING(TIME,"HH:MM:SS"),":","")
       lcLogFile    = lcOutDir + "check_conv_order_icc_" + lcToday + ".log".

OUTPUT STREAM strout TO lcLogFile.

PUT STREAM strout UNFORMATTED 
   "OrderId;CERRADA Status TimeStamp" SKIP.

FOR EACH Order NO-LOCK WHERE 
         Order.Brand      EQ Syst.Var:gcBrand                              AND 
  LOOKUP(Order.StatusCode,{&ORDER_STATUS_PENDING_ICC_FROM_INSTALLER}) EQ 0 AND  
         Order.ICC        EQ "":               

   FIND FIRST EventLog NO-LOCK WHERE
              EventLog.TableName            EQ "Order"                                    AND
              EventLog.Key                  EQ "1" + CHR(255) + STRING(Order.OrderId)     AND
              EventLog.Action               EQ "Modify"                                   AND 
      ENTRY(3,EventLog.Datavalues,CHR(255)) EQ {&ORDER_STATUS_PENDING_ICC_FROM_INSTALLER} NO-ERROR.

   IF NOT AVAIL EventLog THEN DO:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
             ErrorLog.ActionID  = "CERRADASTATUS"
             ErrorLog.TableName = "Order"
             ErrorLog.KeyValue  = STRING(Order.OrderId) 
             ErrorLog.ErrorMsg  = "Cerrada status eventlog is not available"
             ErrorLog.UserCode  = Syst.Var:katun
             ErrorLog.ActionTS  = Func.Common:mMakeTS().
      NEXT.
   END.

   ldeCerradaTS = Func.Common:mHMS2TS(EventLog.EventDate,EventLog.EventTime).

   ASSIGN ldeCerrada = Func.Common:mOffSet(ldeCerradaTS,12)
          ldeCurrent = Func.Common:mOffSet(ldeCurrentTS,0).

   IF ldeCerrada < ldeCurrent THEN DO:
      IF llDoEvent THEN DO:
         lhOrder = BUFFER Order:HANDLE.
         RUN StarEventInitialize(lhOrder).
         RUN StarEventSetOldBuffer(lhOrder).
      END.
   
      fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_SENDING_TO_LO}).

      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrder).
         fCleanEventObjects().
      END.
      
      PUT STREAM strout UNFORMATTED 
         Order.OrderId ";"
         ldeCerradaTS  SKIP.
   END.

END.      

OUTPUT STREAM strout CLOSE.
