/* fprevoper.i     01.07.05/aam
   get the operator, where subscription was transferred from 
*/

FUNCTION fPrevOperator RETURNS CHARACTER
   (iiMsSeq AS INT).

   DEF VAR lcOperName AS CHAR NO-UNDO.
   DEF VAR liMPos     AS INT  NO-UNDO. 
   
   lcOperName = "".
   IF lcOperName = "" THEN 
   FOR FIRST Order NO-LOCK WHERE
             Order.MsSeq = iiMsSeq  AND
             Order.Stat  = "6" AND
             Order.OrderType < 2:
      lcOperName = Order.CurrOper.
   END.
   
   RETURN lcOperName.
             
END FUNCTION.


