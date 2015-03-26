def var ldadate as date no-undo.

def stream slog.
output stream slog to /apps/snet/200909/aam_yob217.log.

put stream slog unformatted 
   "Date"   chr(9)
   "CC"     chr(9)
   "CLI"    chr(9)
   "MSCID"  chr(9)
   "Error"  chr(9)
   "BDest"  chr(9)
   "BillItem" chr(9)
   "Duration" chr(9)
   "Amt"    skip.
   
do ldadate = 6/1/9 to today:

   for each mobcdr no-lock where
            mobcdr.datest = ldadate and
            mobcdr.gsmbnr = "333089":

      /*
      disp mobcdr.datest
           mobcdr.spocmt format ">>9" 
           mobcdr.cli   format "x(10)"
           mobcdr.mscid format "x(5)"
           mobcdr.errorcode
           mobcdr.bdest
           mobcdr.billcode
           mobcdr.amount.
      */
           
      put stream slog unformatted
         mobcdr.datest chr(9)
         mobcdr.spocmt chr(9)
         mobcdr.cli   chr(9)
         mobcdr.mscid chr(9)
         mobcdr.errorcode chr(9)
         mobcdr.bdest chr(9)
         mobcdr.billcode chr(9)
         mobcdr.billdur chr(9)
         mobcdr.amount skip.
         
   end.
end.
