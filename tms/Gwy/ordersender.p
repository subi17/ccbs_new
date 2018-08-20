/* ----------------------------------------------------------------------------
  MODULE .......: Ordersender.p
  FUNCTION .....: Loop for Order requests
  APPLICATION ..: TMS
  CREATED ......: 
  CHANGED ......: 16.08.07/aam tokenlib removed,
                               output order quantity
  Version ......: TMS
  --------------------------------------------------------------------------- */

/**  
     this module is run from orderrun.p 
****/

{Syst/commali.i}
{Func/cparam2.i}
{Syst/eventval.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/heartbeat.i}
{Func/forderstamp.i}
{Func/orderfunc.i}
{Func/freacmobsub.i}
{Func/fixedlinefunc.i}
{Func/multitenantfunc.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}
END.

DEFINE INPUT  PARAMETER piOrderId  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER oiOrderQty AS INT     NO-UNDO.

DEFINE VARIABLE ldToday       AS DATE      NO-UNDO.
DEFINE VARIABLE lcTime        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStock       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeSwitchTS   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liLoop        AS INTEGER   NO-UNDO.
DEFINE VARIABLE liPause       AS INTEGER   NO-UNDO.
DEFINE VARIABLE liAlive       AS INTEGER   NO-UNDO.
DEFINE VARIABLE llOK          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llQuit        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcReturn      AS CHARACTER NO-UNDO INIT "ERROR".
DEFINE VARIABLE liNagios      AS INTEGER   NO-UNDO.
DEFINE VARIABLE ocError       AS CHARACTER NO-UNDO.
DEFINE STREAM sLog.
DEFINE VARIABLE ocResult      AS CHAR      NO-UNDO.
DEFINE VARIABLE ok            AS LOG       NO-UNDO.
DEFINE VARIABLE lcStatus      AS CHAR      NO-UNDO.
DEF VAR liRequestID AS INT NO-UNDO. 

DEFINE BUFFER xxOrder  FOR Order.
DEF BUFFER bOldOrder FOR Order.
DEF BUFFER bSIM FOR SIM.

DEFINE VARIABLE lii           AS INT       NO-UNDO.
DEFINE VARIABLE lcStatuses    AS CHAR NO-UNDO EXTENT 4 INITIAL ["1","3","30","77"].

IF piOrderID = 0 THEN DO lii = 1 to EXTENT(lcStatuses):

   lcStatus = lcStatuses[lii].
   
   LOOP:
   FOR EACH xxOrder NO-LOCK WHERE  
            xxOrder.Brand      = Syst.Var:gcBrand     AND
            xxOrder.StatusCode = lcStatus
      TENANT-WHERE BUFFER-TENANT-ID(xxOrder) GE 0:
   
      IF NOT fsetEffectiveTenantForAllDB(BUFFER-TENANT-NAME(xxOrder))
      THEN NEXT.

      FIND FIRST Order WHERE 
            RECID(Order) = RECID(xxOrder) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            
      IF LOCKED(Order) THEN NEXT.    
      
      IF CAN-FIND(FIRST OrderProduct WHERE OrderProduct.OrderID = Order.OrderID) THEN 
          RUN Mm/orderproduct_exec.p(INPUT Order.OrderID).
      ELSE IF Order.StatusCode = {&ORDER_STATUS_PENDING_FIXED_LINE} THEN 
          NEXT.    
      ELSE
      DO:
          {Mc/ordersender.i LOOP}
      END.   
   
      oiOrderQty = oiOrderQty + 1.
 
      RELEASE MSISDN.

   END.     
END.

ELSE DO: 

   ORDERLOOP:
   FOR FIRST Order EXCLUSIVE-LOCK WHERE  
             Order.Brand   = Syst.Var:gcBrand  AND
             Order.OrderId = piOrderId:
                 
       IF CAN-FIND(FIRST OrderProduct WHERE OrderProduct.OrderID = Order.OrderID) THEN 
           RUN Mm/orderproduct_exec.p(INPUT Order.OrderID).
       ELSE IF Order.StatusCode = {&ORDER_STATUS_PENDING_FIXED_LINE} THEN 
           NEXT.    
       ELSE
       DO:
           {Mc/ordersender.i ORDERLOOP}
       END.
   
      oiOrderQty = oiOrderQty + 1.
      
      RELEASE MSISDN.

   END.     
END.
  
