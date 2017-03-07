{Syst/testpaa.i}
gcbrand = "1".
katun = "YoigoRequest".
{Func/fcreditreq.i}
{Func/timestamp.i}

session:numeric-format = "european".

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo. 
def var lireq as int no-undo.
def var lcerror as char no-undo.
def var lcline as char no-undo.
def var lcinv as char no-undo.
def var lccli as char no-undo.
def var limsseq as int no-undo.
def var ldamt as dec  no-undo.

def stream sread.
input stream sread from /apps/snet/200812/credit_denied_081204.txt.

def stream slog.
output stream slog to /apps/snet/200812/aam_ycm1133.log append.

repeat:

   import stream sread unformatted lcline.
   
   assign
      limsseq = integer(entry(1,lcline,chr(9)))  
      lcinv   = entry(2,lcline,chr(9))
      ldamt   = decimal(entry(3,lcline,chr(9)))
      lccli   = entry(4,lcline,chr(9))
      no-error.
   if error-status:error then next.
     
   find first invoice where 
              invoice.brand = "1" and
              invoice.extinvid = lcinv no-lock no-error.
   if not available invoice then next.

   if invoice.invdate ne 12/1/8 or
      invoice.invtype ne 1 or
      invoice.cli ne lccli or
      invoice.msseq ne limsseq or
      invoice.invamt ne ldamt
   then do:
      message "check" lcinv lccli
      view-as alert-box.
      next.
   end.

   i = i + 1.

   if invoice.crinvnum > 0 then do:
      message "credited" lcinv lccli
      view-as alert-box.
      next. 
   end.

   if invoice.paymstate ne 0 then do:
      message "paid" lcinv lccli invoice.paymstate
      view-as alert-box.
      next. 
   end.
  
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
             Memo.MemoText  = "Credited by Yoigo, YCM-1132".
             Memo.CreStamp  = fMakeTS().
   end.
   
   pause 0.
   disp i k invoice.extinvid with 1 down.

end.

output stream slog close.

session:numeric-format = "american".

disp i k. 




