def var ldafrom as date no-undo.
def var ldato as date no-undo.

ldafrom = 9/1/9.
ldato = 12/31/49.

find first termmobsub where 
        cli = "672527016" no-lock.

disp msseq.
        
For each Limit USE-INDEX MsSeq WHERE
         Limit.MsSeq     = termmobsub.MsSeq  AND
         Limit.LimitType = 3               AND
         Limit.TMRuleSeq = 0               AND
         Limit.LimitID   = 0               AND
         Limit.CustNum   = termmobsub.InvCust NO-LOCK
by limit.todate desc:         
   ldafrom = limit.todate + 1.
   leave.
END.

if ldafrom <= 9/1/9 then do.      
      CREATE Limit.
      ASSIGN 
         Limit.Brand     = "1"
         Limit.MsSeq     = termmobsub.MsSeq   
         Limit.LimitType = 3               
         Limit.TMRuleSeq = 0               
         Limit.FromDate  = ldafrom
         Limit.ToDate    = ldato          
         Limit.LimitID   = 0               
         Limit.CustNum   = termmobsub.InvCust 
         Limit.LimitAmt  = 2
         Limit.ValueType = 1.
end.         
