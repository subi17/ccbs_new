/* ----------------------------------------------------------------------
  MODULE .......: substerminal.i
  TASK .........: 
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 18.01.10
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commali.i}

FUNCTION fCreateSubsTerminal RETURNS INT
   (BUFFER ibOrder FOR Order):

   DEF VAR liTerminalID AS INT NO-UNDO.
   
   IF CAN-FIND(FIRST SubsTerminal WHERE
               SubsTerminal.Brand = gcBrand AND
               SubsTerminal.OrderID = ibOrder.OrderId NO-LOCK) THEN RETURN 0.
   
   FIND LAST SubsTerminal USE-INDEX TerminalID NO-LOCK NO-ERROR.
   IF AVAILABLE SubsTerminal THEN liTerminalID = SubsTerminal.TerminalID + 1.
   ELSE liTerminalID = 1.


   FOR EACH OrderAccessory OF ibOrder WHERE
            OrderAccessory.IMEI > "" OR
            OrderAccessory.ProductCode > "": 
      
      CREATE SubsTerminal.
      
      REPEAT:
         
         SubsTerminal.TerminalID = liTerminalID NO-ERROR.
         
         VALIDATE SubsTerminal NO-ERROR.

         IF ERROR-STATUS:ERROR OR SubsTerminal.TerminalID = 0 THEN DO:
            liTerminalID = liTerminalID + 1.
            NEXT.
         END.
         ELSE LEAVE.
      END.
       
      BUFFER-COPY OrderAccessory TO SubsTerminal.
      
      ASSIGN
         SubsTerminal.MsSeq      = ibOrder.MsSeq
         SubsTerminal.PurchaseTS = ibOrder.CrStamp
         SubsTerminal.BillCode   = OrderAccessory.ProductCode.
         
      RELEASE SubsTerminal.   
   END.

   RETURN liTerminalID.
   
END FUNCTION.
