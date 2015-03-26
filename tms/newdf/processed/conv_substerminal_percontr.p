def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.

def buffer borigreq for msrequest.

for each dccli no-lock,
   first msrequest no-lock where
         msrequest.msseq = dccli.msseq and
         msrequest.reqtype = 8 and
         msrequest.reqstat = 2 and
         msrequest.reqcparam3 = dccli.dcevent,
   first daycampaign no-lock where
         daycampaign.brand = "1" and
         daycampaign.dcevent = dccli.dcevent:
         
      i = i + 1.
         
      IF DayCampaign.DCType = "3" AND 
         LOOKUP(MsRequest.ReqSource,",1,7") > 0 THEN DO:

         j = j + 1.
         
         /* new subscription */
         IF lookup(MsRequest.ReqSource,",1") > 0 THEN 
            FIND FIRST Order WHERE 
                       Order.MsSeq = MsRequest.MsSeq AND
                       Order.OrderType NE 2 NO-LOCK NO-ERROR.
         /* aftersales (renove) */
         ELSE DO:
            FIND FIRST bOrigReq WHERE 
                       bOrigReq.MsRequest = MsRequest.OrigReq NO-LOCK NO-ERROR.
            IF AVAILABLE bOrigReq THEN 
               FIND FIRST Order WHERE
                          Order.Brand   = "1" AND
                          Order.OrderID = bOrigReq.ReqIParam1 NO-LOCK NO-ERROR.
         END.
         
         IF AVAILABLE Order THEN DO:
            FIND FIRST SubsTerminal WHERE   
                       SubsTerminal.Brand   = "1" AND
                       SubsTerminal.OrderID = Order.OrderID AND
                       SubsTerminal.MsSeq   = MsRequest.MsSeq 
               NO-LOCK NO-ERROR.
            IF AVAILABLE SubsTerminal AND SubsTerminal.PerContractID = 0 
            THEN DO:
               FIND CURRENT SubsTerminal EXCLUSIVE-LOCK.
               SubsTerminal.PerContractID = DCCLI.PerContractID.
               RELEASE SubsTerminal.
               k = k + 1.
            END.
         END.
      
      END.

      pause 0.
      disp i j k with 1 down.
end.


