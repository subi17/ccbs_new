{timestamp.i}

def var i as int no-undo.

for each invoice no-lock use-index invdate where
         invoice.brand = "1" and
         invoice.invdate = 7/1/8 and
         invoice.invtype = 1 and
         invoice.invcfg[1] = true and
         invoice.crinvnum > 0:

   CREATE Memo.
   ASSIGN Memo.Brand     = "1"
          Memo.HostTable = "Invoice"
          Memo.KeyValue  = string(invoice.crinvnum)
          Memo.CustNum   = invoice.CustNum
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = "YoigoRequest" 
          Memo.MemoTitle = "Erroneous Invoice"
          Memo.MemoText  = "Credited by Yoigo, YCM-717".
          Memo.CreStamp  = fMakeTS().
    
   i = i + 1.
   pause 0.
   disp i with 1 down.
end.
