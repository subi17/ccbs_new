def buffer bfat for fatime.
def buffer xfatime for fatime.

def var ldused as dec no-undo.
def var i as int no-undo.
def var ldbill as dec no-undo.
def var ldtrans as dec no-undo.

for each fatime no-lock where
     brand = "1" and
     hosttable = "" and
     keyvalue = "YOT-207" and
     origfat = 0 and
     amt < 25,
  first fatgroup no-lock where
        fatgroup.brand = "1" and 
        fatgroup.ftgrp = fatime.ftgrp:

         i = i + 1.
         
         disp i cli fatime.ftgrp fatime.period fatime.amt
                can-find(first bfat where 
                        bfat.brand = "1" and bfat.origfat = fatime.fatnum).  

         ldused = 0.
         for each bfat no-lock use-index cli_s where
                  bfat.cli = fatime.cli and
                  bfat.fatnum < fatime.fatnum and
                  bfat.ftgrp = fatime.ftgrp and
                  bfat.origfat = 0:
            ldused = ldused + bfat.amt.
         end.
         
         ldbill = 0.
         if fatime.invnum > 0 then 
         for first invoice no-lock where
                   invoice.invnum = fatime.invnum,
              each invrow of invoice no-lock where
                   invrow.billcode = fatgroup.billcode and
                   invrow.rowtype = 7:
            ldbill = ldbill + invrow.amt.        
         end.          
         
         disp ldused ldbill.

        if fatime.amt < 0 and fatime.used = 0 and fatime.transqty = 0 and
           ldbill = 0 then do:
            find bfat where recid(bfat) = recid(fatime) exclusive-lock.
            assign
               bfat.amt = 25
               bfat.invnum = 0.
        end.

        else if not can-find(first bfat where 
                        bfat.brand = "1" and bfat.origfat = fatime.fatnum) and
           ldbill = -1 * fatime.amt and
           fatime.transqty = 0 
        then do:
            
            find bfat where recid(bfat) = recid(fatime) exclusive-lock.
            assign
               ldtrans = 25 - bfat.amt
               bfat.amt = 25
               bfat.transqty = ldtrans.
            
            CREATE xFATime.
            BUFFER-COPY bFAT EXCEPT bFAT.FATnum TO xFATime.
         
            ASSIGN xFATime.FatNum      = NEXT-VALUE(ftseq)
                   xFATime.OrigFat     = bFAT.FatNum
                   xFATime.TransPeriod = bFAT.Period
                   xFatime.Amt         = ldtrans
                   xFATime.Invnum      = 0
                   xFATime.Used        = 0
                   xFATime.TransQty    = 0.
        end.

        else if can-find(first bfat where 
                        bfat.brand = "1" and bfat.origfat = fatime.fatnum) and
           -1 * ldbill < fatime.amt and
           fatime.transqty > 0 
        then do:
            
            find bfat where recid(bfat) = recid(fatime) exclusive-lock.
            assign
               ldtrans = 25 - bfat.amt
               bfat.amt = 25
               bfat.transqty = bfat.transqty + ldtrans.
            
            find first bfat where 
                       bfat.brand = "1" and 
                       bfat.origfat = fatime.fatnum exclusive-lock.
            bfat.amt = bfat.amt + ldtrans.           
        end.
end.
        
                