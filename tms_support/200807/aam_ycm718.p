{Syst/testpaa.i}
gcbrand = "1".
katun = "YoigoRequest".
{Func/fcreditreq.i}
{Func/timestamp.i}

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo. 
def var lireq as int no-undo.
def var lcerror as char no-undo.

def stream slog.
output stream slog to /apps/snet/200807/aam_ycm718.log append.

for each invoice no-lock use-index invdate where
         invoice.brand = "1" and
         invoice.invdate = 7/1/8 and
         invoice.invtype = 1 and
         invoice.invcfg[1] = true:

   i = i + 1.

   if invoice.crinvnum > 0 then next. 
    
   for first singlefee exclusive-lock where
             singlefee.invnum = invoice.invnum and
             singlefee.billcode = "termperiod" and
             singlefee.amt = 100:

      export stream slog singlefee.
      delete singlefee.
      j = j + 1.
   end.

   lireq = fFullCreditNoteRequest(Invoice.Custnum,
                                  Invoice.InvNum,
                                  "2013",     /* Reason */
                                  0,          /* no returns */
                                  TRUE,       /* release */
                                  0,          /* act now */
                                  FALSE,      /* sms */
                                  "",
                                  OUTPUT lcError).

   if lireq = 0 then 
      message "request failed for " invoice.invnum  ":" lcerror
      view-as alert-box.
      
   else do:
      k = k + 1.    
    
      find msrequest where msrequest.msrequest = lireq no-lock.  
      CREATE Memo.
      ASSIGN Memo.Brand     = "1"
             Memo.HostTable = "MsRequest"
             Memo.KeyValue  = string(msrequest.msrequest)
             Memo.CustNum   = msrequest.CustNum
             Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
             Memo.CreUser   = msrequest.usercode 
             Memo.MemoTitle = "Erroneous Invoice"
             Memo.MemoText  = "Credited by Yoigo, YCM-717".
             Memo.CreStamp  = fMakeTS().
   end.

   pause 0.
   disp i j k invoice.extinvid with 1 down.
end.

