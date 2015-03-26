def stream slog.
output stream slog to /apps/snet/201001/yot459_haiti_cdr.txt.

def var ldadate as date no-undo.

def temp-table ttcli no-undo
   field cli as char 
   field spocmt as int
   field datest as date
   field qty as int
   field amt as dec
   index cli cli datest spocmt.

put stream slog unformatted
   "Date"     chr(9)
   "MSISDN"   chr(9)
   "CallCase" chr(9)
   "BNumber"  chr(9)
   "BDestination" chr(9)
   "Amount"   skip.
   
session:numeric-format = "european".

do ldadate = 1/1/10 to 1/25/10:

   for each mobcdr no-lock use-index gsmbnr where
            mobcdr.datest = ldadate and
            mobcdr.gsmbnr begins "509" and
            mobcdr.btype = 1 and
            mobcdr.errorcode = 0 /* and 
            lookup(string(mobcdr.spocmt),"51,53,2,3") > 0 */:
            /* 54? */
            
      /*
      disp mobcdr.datest
           mobcdr.cli
           mobcdr.spocmt
           mobcdr.gsmbnr
           mobcdr.bdest
           mobcdr.amount.
       */
       
       /*
       find first ttcli where
                  ttcli.cli = mobcdr.cli and
                  ttcli.datest = mobcdr.datest and
                  ttcli.spocmt = mobcdr.spocmt no-error.
       if not available ttcli then do:
          create ttcli.
          assign 
             ttcli.cli = mobcdr.cli
             ttcli.datest = mobcdr.datest
             ttcli.spocmt = mobcdr.spocmt.
       end.
       
       assign 
         ttcli.qty = ttcli.qty + 1
         ttcli.amt = ttcli.amt + mobcdr.amount.
       */
         
       put stream slog unformatted
          string(mobcdr.datest,"99.99.9999") chr(9)
          mobcdr.cli chr(9)
          mobcdr.spocmt chr(9)
          mobcdr.gsmbnr chr(9)
          mobcdr.bdest  chr(9)
          mobcdr.amount skip.
          
   end.
   
end.

/*
put stream slog unformatted
   "MSISDN"   chr(9)
   "Date"     chr(9)
   "CallCase" chr(9)
   "Qty"      chr(9)
   "Amount"   skip.
   
for each ttcli:
   
   put stream slog unformatted
      ttcli.cli   chr(9)
      ttcli.datest chr(9)
      ttcli.spocmt chr(9)
      ttcli.qty    chr(9)
      trim(string(ttcli.amt,"->>>>>>>9.99"))  skip.
end.
*/

output stream slog close.

session:numeric-format = "american".


   

