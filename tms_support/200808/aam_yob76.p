{testpaa.i}
gcbrand = "1".
katun = "YoigoRequest".
{fcreditreq.i}
{timestamp.i}

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo. 
def var lireq as int no-undo.
def var lcerror as char no-undo.

def stream slog.
output stream slog to /apps/snet/200808/aam_yob76.log append.

for each invoice no-lock use-index invdate where
         invoice.brand = "1" and
         invoice.invdate = 8/2/8 and
         invoice.invtype = 1 and
         invoice.chgstamp < 20080802 and
         invoice.paymstate = 0 and
         invoice.ddstate = 0 and
         invoice.printstate = 0:

   i = i + 1.

   if invoice.crinvnum > 0 then next. 

   lireq = fFullCreditNoteRequest(Invoice.Custnum,
                                  Invoice.InvNum,
                                  "2013",     /* Reason */
                                  0,          /* no returns */
                                  FALSE,      /* don't release */
                                  0,          /* act now */
                                  FALSE,      /* sms */
                                  "",
                                  OUTPUT lcError).

   if lireq = 0 then 
      message "request failed for " invoice.invnum  ":" lcerror
      view-as alert-box.
      
   else do:
      k = k + 1.    
    
      put stream slog unformatted
          invoice.invnum  chr(9)
          invoice.extinvid chr(9)
          invoice.cli chr(9)
          lireq  skip.
          
      find msrequest where msrequest.msrequest = lireq no-lock.  
      CREATE Memo.
      ASSIGN Memo.Brand     = "1"
             Memo.HostTable = "MsRequest"
             Memo.KeyValue  = string(msrequest.msrequest)
             Memo.CustNum   = msrequest.CustNum
             Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
             Memo.CreUser   = msrequest.usercode 
             Memo.MemoTitle = "Erroneous Invoice"
             Memo.MemoText  = "Credited by Yoigo, YOB-67".
             Memo.CreStamp  = fMakeTS().
   end.

   pause 0.
   disp i k invoice.extinvid with 1 down.

end.

output stream slog close.




