/* ----------------------------------------------------------------------------
  MODULE .......: Orderrun.p
  FUNCTION .....: screen GUI for Order request sender
  APPLICATION ..: TMS
  CREATED ......: 
  CHANGED ......: 16.08.07/aam order quantity from sender
  Version ......: TMS
  --------------------------------------------------------------------------- */


{commpaa.i}     gcBrand = "1" . katun = "Order".
{heartbeat.i}

DEFINE VARIABLE liLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldToday    AS DATE      NO-UNDO.
DEFINE VARIABLE lcTime     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liAmount   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liNagios   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liOrders   AS INTEGER   NO-UNDO.

FORM
   SKIP
   "ROUND:" liLoop   FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "Order Qty:" liOrders FORMAT ">>>>>>>9" 
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " Yoigo Order sender  "
FRAME frmLog.

                    

  liNagios = fKeepAlive("ords:Order Sender").
  PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                      STRING(time,"hh:mm:ss").
                                         
Yoigo:
DO WHILE TRUE :

   liLoop = liLoop + 1.
   PAUSE 0.
   DISP  liLoop  
         Today @ ldToday 
         liOrders
         string(time,"hh:mm:ss") @ lctime
   WITH FRAME frmLog.
   PAUSE 0.

   RUN ordersender(0,
                   OUTPUT liAmount).

   liOrders = liOrders + liAmount.
  
   PUT SCREEN ROW 22 COL 1
   "F8 TO QUIT, OTHER KEYS START  ORDER SENDER  IMMEDIATELLY".
   
   READKEY PAUSE 60.
   
   liNagios = fKeepAlive("ords:Order Sender").
   PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                      STRING(time,"hh:mm:ss").
   IF KEYLABEL(LASTKEY) = "F8" THEN DO:
       LEAVE Yoigo.
   END.

END.

QUIT.

