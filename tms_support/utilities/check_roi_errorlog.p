
/* check that exist orders with ROI history send in error status */
DEFINE INPUT PARAMETER pcOutFile AS CHAR NO-UNDO.

DEFINE VARIABLE lcBrand AS CHARACTER NO-UNDO INITIAL "1". 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE STREAM sout.

OUTPUT STREAM sout TO VALUE(pcOutFile).

FOR EACH Order NO-LOCK WHERE
         Order.Brand = lcBrand AND
         Order.SendToROI = 3 :

   FIND LAST ErrorLog NO-LOCK WHERE
             ErrorLog.Brand = lcBrand AND
             ErrorLog.TableName = "Order" AND
             ErrorLog.KeyValue = STRING(Order.OrderId) AND
             ErrorLog.ActionID = "ROIHistory" NO-ERROR. 
   IF AVAIL ErrorLog THEN DO:
   lcLine = STRING(TODAY) +  
            " OrderId: " + STRING(Order.OrderId) + 
            " ActionTS: " + STRING(ErrorLog.ActionTS) + 
            " Error: " + ErrorLog.ErrorMsg .
   PUT STREAM sout UNFORMATTED lcLine SKIP.
   END.
END.

OUTPUT STREAM sout CLOSE.
