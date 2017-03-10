{Syst/testpaa.i}
{/home/ari/test/fcreditreq.i}

def var i         as int  no-undo.
def var j         as int  no-undo.
def var ldcurrent as dec  no-undo.
def var lcreason  as char no-undo.
def var lcerror   as char no-undo.

def buffer binv for invoice.

def temp-table ttreq no-undo
   field invnum as int
   index invnum invnum.

   
for each msrequest no-lock where
         msrequest.brand = "1" and
         msrequest.reqtype = 22:
   create ttreq.
   ttreq.invnum = msrequest.reqiparam1.
   i = i + 1.
end.

message i "requests"
view-as alert-box.

i = 0.

ldcurrent = fmakets().
lcreason  = "2013". 

for each order no-lock where
         order.brand = "1" and
         order.source = "script" and
         order.clitype = "tarj3",
   first invoice no-lock where
         invoice.invnum = order.invnum and
         invoice.crinvnum = 0 and
         invoice.paymstate = 0
by order.orderid:
         

   if can-find(first ttreq where ttreq.invnum = invoice.invnum) then next.
    
   i = i + 1.
   
   if i mod 500 = 0 then do:
      pause 0.
      disp i with 1 down.
      
      ldcurrent = ldcurrent + 0.005.
   end.

   /* make a request */
   j = fFullCreditNoteRequest(Invoice.Custnum,
                              Invoice.InvNum,
                              lcReason,
                              0,          /* no refunds */
                              false,      /* no event releases */
                              ldcurrent,
                              FALSE,      /* sms */
                              "",
                              OUTPUT lcError).

   IF j > 0 THEN DO trans:
   
      /* make sure that invoice cannot be printed */
      IF Invoice.PrintState = 0 THEN DO:
         FIND bInv where recid(binv) = recid(invoice) EXCLUSIVE-LOCK.
         bInv.InvCfg[1] = TRUE.
      END.

      CREATE Memo.
      ASSIGN
      Memo.CreStamp  = fMakeTS()
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.Brand     = gcBrand
      Memo.MemoTitle = "Credit Reason"
      Memo.CreUser   = katun
      Memo.HostTable = "Invoice"
      Memo.KeyValue  = STRING(Invoice.InvNum)
      Memo.CustNum   = Invoice.CustNum
      Memo.Memotext  = "Credited based on Yoigo's decision / " +
                       "Ove Wik 12th of December 2007. Erroneus tax " +
                       "handling for Pre-activated Prepaid subscriptions.".

      create ttreq.
      ttreq.invnum = invoice.invnum.
   
   end.

   else do:
      message "Request creation failed;" skip
              lcerror
      view-as alert-box error.
   end. 

   if i > 20000 then leave. 
end.

disp i.


