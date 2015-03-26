def var i as int no-undo.
def var j as int no-undo.

for each order no-lock use-index orderid:

   i = i + 1.
   if i mod 1000 = 0 then do:
      pause 0.
      disp i j with 1 down.
   end.
   
   if can-find(first substerminal where
                     substerminal.brand = "1" and
                     substerminal.orderid = order.orderid)
   then next.

   run pcreateterminal.
end.

disp i j .


PROCEDURE pCreateTerminal:

   DEF VAR liTerminalID AS INT  NO-UNDO.
   
   FIND LAST SubsTerminal USE-INDEX TerminalID NO-LOCK NO-ERROR.
   IF AVAILABLE SubsTerminal THEN liTerminalID = SubsTerminal.TerminalID + 1.
   ELSE liTerminalID = 1.
   
   FOR EACH OrderAccessory OF Order NO-LOCK WHERE
            OrderAccessory.IMEI > "" OR
            OrderAccessory.ProductCode > "": 
   
      CREATE SubsTerminal.
      
      REPEAT:
         SubsTerminal.TerminalID = liTerminalID NO-ERROR.
         
         VALIDATE SubsTerminal.

         IF ERROR-STATUS:ERROR OR SubsTerminal.TerminalID = 0 THEN DO:
            liTerminalID = liTerminalID + 1.
            NEXT.
         END.
         ELSE LEAVE.
      END.
       
      BUFFER-COPY OrderAccessory TO SubsTerminal.
      
      ASSIGN
         SubsTerminal.MsSeq      = Order.MsSeq
         SubsTerminal.PurchaseTS = Order.CrStamp
         SubsTerminal.BillCode   = OrderAccessory.ProductCode.
   
      j = j + 1.   
      RELEASE SubsTerminal.   
   END.
   
END PROCEDURE.

