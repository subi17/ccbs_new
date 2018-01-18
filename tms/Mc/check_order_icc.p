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

DEF VAR lcToday      AS CHAR     NO-UNDO. 
DEF VAR lcLogFile    AS CHAR     NO-UNDO. 
DEF VAR lcOutDir     AS CHAR     NO-UNDO.
DEF VAR ldeCurrentTS AS DEC      NO-UNDO.
DEF VAR ldtCerradaDT AS DATETIME NO-UNDO.
DEF VAR ldtCurrentDT AS DATETIME NO-UNDO.
DEF VAR lhOrder      AS HANDLE   NO-UNDO.

DEF STREAM strout. 

FUNCTION mTS2DateTime RETURNS DATETIME
   (ideTS AS DECIMAL):

   DEFINE VARIABLE liYY    AS INTEGER  NO-UNDO.
   DEFINE VARIABLE liMM    AS INTEGER  NO-UNDO.
   DEFINE VARIABLE liDD    AS INTEGER  NO-UNDO.
   DEFINE VARIABLE ldaDate AS DATE     NO-UNDO.
   DEFINE VARIABLE liTime  AS INTEGER  NO-UNDO.

   ASSIGN
      liYY    = TRUNCATE(ideTS,0)
      liTime  = (ideTS - liYY) * 100000000
      liMM    = liYY MOD 10000
      liDD    = liMM MOD 100
      liYY    = (liYY - liMM) / 10000
      liMM    = (liMM - liDD) / 100 
      ldaDate = DATE(liMM,liDD,liYY)
   NO-ERROR.

   IF ERROR-STATUS:ERROR
   THEN RETURN ?.

   RETURN DATETIME(ldaDate, liTime).

END FUNCTION.   

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
         Order.Brand EQ Syst.Var:gcBrand AND 
         Order.ICC   EQ "":
   
   IF NOT Order.CLIType BEGINS "CONTFH" THEN NEXT.

   IF NOT fIsConvergenceTariff(Order.CLIType) THEN NEXT. 

   IF LOOKUP(Order.StatusCode,{&ORDER_LOGISTICS_STATUSES}) > 0 OR 
      LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES})  > 0 THEN NEXT.

   IF CAN-FIND(FIRST OrderFusion NO-LOCK WHERE
                     OrderFusion.Brand        EQ Syst.Var:gcBrand AND
                     OrderFusion.OrderID      EQ Order.OrderID    AND 
                     OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_FINALIZED}) THEN DO:
      
      FIND FIRST FusionMessage NO-LOCK WHERE 
                 FusionMessage.OrderId     EQ Order.OrderId AND 
                 FusionMessage.FixedStatus EQ "CERRADA"     NO-ERROR.

      IF NOT AVAIL FusionMessage THEN NEXT. 
               
      ASSIGN ldtCerradaDT = ADD-INTERVAL(mTS2DateTime(FusionMessage.FixedStatusTS),12,"hours")      
             ldtCurrentDT = mTS2DateTime(ldeCurrentTS).

      IF ldtCerradaDT < ldtCurrentDT THEN DO:
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
            FusionMessage.OrderId       ";"
            FusionMessage.FixedStatusTS SKIP.
   
      END.
         
   END.                  

END.

OUTPUT STREAM strout CLOSE.
