{commpaa.i}
gcBrand = "1".
{tmsconst.i}
{fmakemsreq.i}
{timestamp.i}
{fsubsterminal.i}
DEF VAR lcCounter AS INT NO-UNDO.
DEF VAR lcOrders AS CHAR NO-UNDO.
DEF VAR liLoop AS INT NO-UNDO.
DEF VAR liCreated AS INT NO-UNDO.
DEF VAR lcResult AS CHAR NO-UNDO.
DEF VAR ldaValidTo AS DATE NO-UNDO.
DEF VAR ldaValidFrom AS DATE NO-UNDO.
DEF VAR liBillPeriod AS INT NO-UNDO.

DEF STREAM outfile.

lcCounter = 1.
ldaValidTo = 05/25/16.

FOR EACH Order NO-LOCK WHERE Order.brand EQ "1" AND
                             Order.statuscode EQ "6" AND
                             Order.CrStamp < 20140415 AND
                             Order.Paytype EQ FALSE:

   IF CAN-FIND (FIRST OrderAccessory WHERE 
                      OrderAccessory.Brand EQ "1" AND
                      OrderAccessory.orderid EQ Order.orderid) THEN NEXT.
   
   IF CAN-FIND (FIRST DCCLI WHERE
                      DCCLI.Brand EQ "1" AND
                      DCCLI.dcevent BEGINS "PAYTERM" AND
                      DCCLI.msseq = Order.MsSeq AND
                      DCCLI.ValidTo >= TODAY) THEN NEXT.

   IF NOT CAN-FIND (FIRST MobSub WHERE
                      Mobsub.Brand EQ "1" AND
                      Mobsub.CLI EQ Order.CLI AND
                      MobSub.Paytype EQ FALSE) THEN NEXT.

   IF lcOrders EQ "" THEN lcOrders = STRING(Order.orderId).
   ELSE
      lcOrders = lcOrders + "," + STRING(Order.orderId).
   lcCounter = lcCounter - 1.
   IF lcCounter EQ 0 THEN LEAVE.
END.                     

MESSAGE lcOrders VIEW-AS ALERT-BOX.


DO liLoop = 1 TO NUM-ENTRIES(lcOrders):
   FIND FIRST Order WHERE
              Order.brand = "1" AND
              Order.Orderid EQ INT(ENTRY(liLoop, lcOrders)) NO-LOCK NO-ERROR.
   IF NOT AVAIL order THEN NEXT.
   DO TRANSACTION:
      CREATE OrderAccessory.
      ASSIGN
         OrderAccessory.OrderId       = INT(ENTRY(liLoop, lcOrders))
         OrderAccessory.TerminalType  = {&TERMINAL_TYPE_PHONE}
         OrderAccessory.brand         = "1"
         OrderAccessory.IMEI          = "111122223333"
         OrderAccessory.discount      = 0
         OrderAccessory.Model         = "Q25Phone"
         OrderAccessory.Manufacturer  = "Qvantel"
         OrderAccessory.ModelColor    = "Black"
         OrderAccessory.HardBook      = 1
         OrderAccessory.ProductCode   = "P034S6EN2".

      RELEASE OrderAccessory.

      FIND FIRST Order NO-LOCK WHERE Order.brand = "1" AND
                             Order.orderId = INT(ENTRY(liLoop, lcOrders)) NO-ERROR.
      IF AVAIL Order THEN DO:
         liCreated = fPCActionRequest(Order.MsSeq,
                                      "PAYTERM24_15",
                                      "act",
                                      fMakeTS(),
                                      TRUE, /* create fees */
                                      {&REQUEST_SOURCE_NEWTON},
                                      "",
                                      0,
                                      TRUE,
                                      "",
                                      100,
                                      0,
                                      OUTPUT lcResult).
         
         IF liCreated = 0 THEN DO:
            MESSAGE "Request creation failed: " + lcResult VIEW-AS ALERT-BOX.
            undo, return.
         END.
      END.
      ELSE
         MESSAGE "order failed" VIEW-AS ALERT-BOX.
   END.
   DO liLoop = 1 TO NUM-ENTRIES(lcOrders) TRANSACTION:
      PAUSE 15.
      FIND FIRST Order NO-LOCK WHERE Order.brand = "1" AND
                                 Order.orderId = INT(ENTRY(liLoop, lcOrders)).
      /* Message "BOX" VIEW-AS ALERT-BOX. */
      FIND FIRST DCCLI WHERE
                         DCCLI.Brand EQ "1" AND
                         DCCLI.dcevent BEGINS "PAYTERM" AND
                         DCCLI.msseq = Order.MsSeq AND
                         DCCLI.ValidTo >= TODAY NO-ERROR.
      
      IF AVAIL DCCLI THEN DO:
         /* Message "DCCLI Found" VIEW-AS ALERT-BOX. */
         ldaValidFrom = ADD-INTERVAL(ldaValidTo, -23, 'months':U).
         ldaValidFrom = ADD-INTERVAL(ldaValidFrom, 1, 'days':U).
         liBillPeriod = YEAR(ldaValidTo) * 100 + MONTH(ldaValidTo).

         DCCLI.validFrom = ADD-INTERVAL(DCCLI.ValidFrom, -23, 'months':U).
         DCCLI.validTo = ADD-INTERVAL(DCCLI.ValidTo, -23, 'months':U).
         
         FIND SingleFee USE-INDEX Custnum WHERE
              SingleFee.Brand       = "1" AND
              SingleFee.Custnum     =  Order.custnum AND
              SingleFee.HostTable   = "Mobsub" AND
              SingleFee.SourceTable = "DCCLI"  AND
              SingleFee.SourceKey   = STRING(DCCLI.PerContractId) AND
              SingleFee.CalcObj     = "RVTERM" AND
              SingleFee.billperiod  > liBillPeriod NO-ERROR.
         IF AVAIL SingleFee THEN DO:
            SingleFee.billperiod = liBillPeriod.
            If SingleFee.orderid LE 0 THEN
               SingleFee.orderid = INT(ENTRY(liLoop, lcOrders)).
            RELEASE SingleFee.
         END.
         ELSE
            Message "Singlefee not found." VIEW-AS ALERT-BOX.

         fCreateSubsTerminal(BUFFER order).

/*         CEATE SubsTerminal.
         ASSIGN
            SubsTerminal.OrderId       = INT(ENTRY(liLoop, lcOrders))
            SubsTerminal.TerminalType  = {&TERMINAL_TYPE_PHONE}
            SubsTerminal.brand         = "1"
            SubsTerminal.IMEI          = "111122223333"
            SubsTerminal.Model         = "Q25Phone"
            SubsTerminal.Manufacturer  = "Qvantel"
            SubsTerminal.ModelColor    = "Black"
            SubsTerminal.billcode      = "P034S6EN2"
            SubsTerminal.msseq         = dccli.msseq
            Substerminal.percontractid = DCCLI.PerContractId.
*/
         FIND FixedFee WHERE
              FixedFee.Brand = "1" AND
              FixedFee.billcode EQ "PAYTERM" AND
              FixedFee.custNum EQ Order.custnum AND
              FixedFee.begPeriod EQ YEAR(TODAY) * 100 + MONTH(TODAY).
              
         IF AVAIL FixedFee THEN DO:
            FixedFee.begPeriod = YEAR(ldaValidFrom) * 100 + MONTH(ldaValidFrom).
            FixedFee.endPeriod = YEAR(ldaValidTo) * 100 + MONTH(ldaValidTo).
            FixedFee.begdate = ldavalidfrom.
            Fixedfee.orderid = order.orderid.
            RELEASE FixedFee.
         END.
         ELSE
            Message "FixedFee not found." VIEW-AS ALERT-BOX.
         
         OUTPUT STREAM outfile to VALUE("modified_orders.txt") APPEND.
         PUT STREAM outfile UNFORMATTED
           lcOrders + ";" + STRING(order.cli) + ";" + STRING(order.msseq) SKIP.
         OUTPUT STREAM outfile CLOSE. 
         LEAVE.
      END.   
      ELSE
         IF liLoop = 10 THEN Message "Creation failed" VIEW-AS ALERT-BOX.
      
   END.
END.

