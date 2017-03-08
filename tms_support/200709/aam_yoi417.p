{Func/timestamp.i}
{Func/ftaxdata.i}

def var lcdir   as char no-undo.
def var i       as int  no-undo.
def var lcsep   as char no-undo init "|".
def var ldtdate as date no-undo.
def var litime  as int  no-undo.
def var ldamt   as dec  no-undo.
def var ldvat   as dec  no-undo.
def stream slog.

lcdir = "/store/riftp/dumpfiles/dbdumb/spool/".


output stream slog to value(lcdir + "postpaid_compensation_august_2007.dump").

for first fatgroup no-lock where
          fatgroup.brand  = "1" and
          fatgroup.ftgrp = "comp75",
    first billitem no-lock where
          billitem.brand = "1" and
          billitem.billcode = fatgroup.billcode,
    each fatime no-lock where
         fatime.brand   = "1"            and
         fatime.ftgrp   = fatgroup.ftgrp and
         fatime.period  = 200708:

   ldamt = fatime.amt.
   
   if fatime.vatincl then do:

      ldvat = 0.
      
      for first msowner no-lock where
                msowner.cli    = fatime.cli and
                msowner.tsend > 20070801    and
                msowner.tsbeg < 20070901,
          first customer no-lock where
                customer.custnum = msowner.custnum:
         if customer.vatusage < 3 then 
            ldvat = fRegionTaxPerc(Customer.Region,
                                   BillItem.TaxClass).
      end.
   
      if ldvat > 0 then
         ldamt = ldamt / (1 + ldvat / 100).
     
   end.

   put stream slog unformatted
      fatime.cli   lcsep
      8/31/7       lcsep
      ldamt        skip.
         
   i = i + 1.
   pause 0.
   disp "FAT" i with 1 down row 1.
   
end.         

output stream slog close.

i = 0.

output stream slog to value(lcdir + "prepaid_initialtopup_august_2007.dump").

for each prepaidrequest no-lock where
         prepaidrequest.brand      = "1"         and
         prepaidrequest.source     = "web order" and
         prepaidrequest.tsrequest >= 20070801    and
         prepaidrequest.tsrequest <  20070901    and
         prepaidrequest.respcode = 0:

   fsplitts(prepaidrequest.tsrequest,
            output ldtdate,
            output litime).
            
   put stream slog unformatted
      prepaidrequest.cli  lcsep
      ldtdate             lcsep
      prepaidrequest.topupamt / 100 skip.

   i = i + 1.
   pause 0.
   disp "OT" i with 1 down row 8.
       
end.

output stream slog close.

i = 0.

output stream slog to value(lcdir + "prepaid_compensation_august_2007.dump").

for each prepaidrequest no-lock where
         prepaidrequest.brand      = "1"         and
         prepaidrequest.source     = "cc"        and
         prepaidrequest.tsrequest >= 20070801    and
         prepaidrequest.tsrequest <  20070901    and
         prepaidrequest.request begins "refill"  and
         prepaidrequest.respcode = 0:

   fsplitts(prepaidrequest.tsrequest,
            output ldtdate,
            output litime).
            
   put stream slog unformatted
      prepaidrequest.cli  lcsep
      ldtdate             lcsep
      prepaidrequest.topupamt / 100 skip.

   i = i + 1.
   pause 0.
   disp "cc" i with 1 down row 16.
       
end.

output stream slog close.


