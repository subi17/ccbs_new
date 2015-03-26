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

{commali.i}
{cparam2.i}
{eventval.i}
{timestamp.i}
{tmsconst.i}
{finvtxt.i}
{fcustdata.i}
{fctchange.i}
{fmakemsreq.i}
{msisdn.i}
{heartbeat.i}
{forderstamp.i}
{orderfunc.i}
{freacmobsub.i}

DEFINE INPUT  PARAMETER piOrderId  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER oiOrderQty AS INT     NO-UNDO.

DEFINE VARIABLE ldToday       AS DATE      NO-UNDO.
DEFINE VARIABLE lcTime        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStock      AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE lcStatuses    AS CHAR NO-UNDO EXTENT 3 INITIAL ["1","3","30"].

DEF VAR lcTestCustomer AS CHAR NO-UNDO.

lcTestCustomer = TRIM(fCParamC("OrderTestCustomer")).
IF lcTestCustomer = ? THEN lcTestCustomer = "".


IF piOrderID = 0 THEN DO lii = 1 to EXTENT(lcStatuses):

   lcStatus = lcStatuses[lii].
   
   LOOP:
   FOR EACH xxOrder NO-LOCK WHERE  
            xxOrder.Brand      = gcBrand     AND
            xxOrder.StatusCode = lcStatus:
   
      FIND FIRST OrderCustomer OF xxOrder WHERE
                 OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
      IF AVAILABLE OrderCustomer AND 
         LOOKUP(OrderCustomer.CustId,lcTestCustomer) > 0 THEN NEXT.
         
      FIND FIRST Order WHERE 
            RECID(Order) = RECID(xxOrder) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            
      IF LOCKED(Order) THEN NEXT.       
      
      {ordersender.i LOOP}
   
      oiOrderQty = oiOrderQty + 1.
 
      RELEASE MSISDN.

   END.     
END.

ELSE DO: 

   ORDERLOOP:
   FOR FIRST Order EXCLUSIVE-LOCK WHERE  
             Order.Brand   = gcBrand  AND
             Order.OrderId = piOrderId:

      {ordersender.i ORDERLOOP}
   
      oiOrderQty = oiOrderQty + 1.
      
      RELEASE MSISDN.

   END.     
END.
  
