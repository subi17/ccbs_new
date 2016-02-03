{Func/timestamp.i}

def var i       as int  no-undo.
def var j       as int  no-undo.
def var ldadate as date no-undo.
def var litime  as int  no-undo.
def var lihour  as int  no-undo.
def var llcdrs  as log  no-undo.
def var ldainvdate as date no-undo.
def var lisubs as int no-undo.
def var licdrqty as int no-undo.
def var liinvqty as int no-undo.               
def var lisubqty as int no-undo.
def var ldstart as dec no-undo.
def var ldend as dec no-undo.
def var ldtstart as datetime no-undo.
def var ldtend as datetime no-undo.
def var lidays as int no-undo.
def var lcdur as char no-undo.
def var lidur as int no-undo.
def var litotinv as int no-undo.
def var litotcdr as int no-undo.
def var litotsub as int no-undo.
def var ldabshour as dec no-undo extent 35.

def temp-table ttstat no-undo
   field ttday  as int 
   field tthour as int column-label "Hour"     format ">9"
   field startstamp as dec 
   field invqty as int column-label "Invoices" 
   field cdrqty as int column-label "CDRs"
   field subqty as int 
   index tthour ttday tthour
   index startstamp startstamp.
    
def temp-table ttrun no-undo
   field ttday as int
   field tthour as int
   field billrun as char
   field invqty as int
   field cdrqty as int
   field subqty as int
   index tthour ttday tthour billrun.

def temp-table ttsum no-undo
   field ttday as int
   field tthour as int
   field startstamp as dec 
   field invqty as int
   field cdrqty as int
   field subqty as int
   index tthour ttday tthour.

ldainvdate = today.

pause 0.
update ldainvdate label "Invoice Date" format "99-99-99"
with overlay row 10 centered side-labels title " Billing Run " frame fdate.
hide frame fdate no-pause.

if ldainvdate = ? then return.

ldstart = 99999999.
llcdrs = true.

for each invoice no-lock use-index invdate where
         brand = "1" and
         invdate = ldainvdate and
         invtype = 1 and
         invoice.chgstamp > 0:

    assign         
       ldstart = min(ldstart,invoice.chgstamp)
       ldend   = max(ldend,invoice.chgstamp).

    j = j + 1.
    if j mod 1000 = 0 then do:
       pause 0.
       disp j label "1. Invoices" with 1 down row 1 overlay frame fqty1.
    end.
 
end.

ldabshour[1] = ldstart.
do j = 2 to extent(ldabshour):
   ldabshour[j] = fSecOffSet(ldabshour[j - 1],3600).
end.

do j = 1 to extent(ldabshour):

   fsplitts(ldabshour[j],
            output ldadate,
            output litime).
 
   create ttstat.
   assign 
      ttstat.ttday  = day(ldadate)
      ttstat.tthour = j
      ttstat.startstamp = ldabshour[j].
end.

j = 0.

for each invoice no-lock use-index invdate where
         brand = "1" and
         invdate = ldainvdate and
         invtype = 1 and
         invoice.chgstamp > 0:

    liinvqty = liinvqty + 1.

    if llcdrs then do:
       i = 0.
       for each invrow of invoice no-lock where
                invrow.rowtype = 2:
           i = i + invrow.qty.
       end.
    end.
      
    fsplitts(invoice.chgstamp,
             output ldadate,
             output litime).
    
    /*
    assign         
       lihour = integer(entry(1,string(litime,"hh:mm:ss"),":")).
    */
    
    find last ttstat where 
              ttstat.startstamp <= invoice.chgstamp no-error.
    if not available ttstat then do:
       message "check stamp:" invoice.extinvid
                invoice.chgstamp
       view-as alert-box.
       next.
    end.

    lisubs = 0.
    for each subinvoice of invoice no-lock:
       lisubs = lisubs + 1.
    end.
    
    assign
       ttstat.invqty = ttstat.invqty + 1
       ttstat.cdrqty = ttstat.cdrqty + i
       ttstat.subqty = ttstat.subqty + lisubs
       lihour        = ttstat.tthour.

    find first ttrun where 
               ttrun.ttday  = day(ldadate) and
               ttrun.tthour = lihour and
               ttrun.billrun = invoice.billrun no-error.
    if not available ttrun then do:
       create ttrun.
       assign 
          ttrun.ttday  = day(ldadate)
          ttrun.tthour = lihour
          ttrun.billrun = invoice.billrun.
    end.
    assign
       ttrun.invqty = ttrun.invqty + 1
       ttrun.cdrqty = ttrun.cdrqty + i
       ttrun.subqty = ttrun.subqty + lisubs.
    
    assign
       licdrqty = licdrqty + i
       lisubqty = lisubqty + lisubs.

    /*
    disp duedate invamt printstate extinvid chgstamp 
        i format "->>>>>>>>9".
    */    
    j = j + 1.
    if j mod 1000 = 0 then do:
       pause 0.
       disp j label "2. Invoices" with 1 down overlay frame fqty2.
    end.
    
end.

hide frame fqty1 no-pause. 
hide frame fqty2 no-pause.

def stream slog.
output stream slog to value("/scratch/log/invoicestat/billrun_abs_stat_" +
                            string(year(ldainvdate),"9999") +
                            string(month(ldainvdate),"99") + 
                            string(day(ldainvdate),"99") + ".txt").

for each ttstat:
   
   if ttstat.invqty = 0 then next. 
    
   display stream slog 
      ttstat.tthour
      ttstat.invqty
      ttstat.cdrqty.
   
   i = 0.
   for each ttrun where           
            ttrun.ttday  = ttstat.ttday and
            ttrun.tthour = ttstat.tthour:
      i = i + 1.
   end.
   
   disp stream slog 
      ttstat.cdrqty / ttstat.invqty 
         column-label "CDRs/inv"
         format ">>,>>>,>>9"
      i column-label "Runs"
      ttstat.invqty / i 
         column-label "Inv./run"
         format ">>>>>>9"
      ttstat.cdrqty / i             
         column-label "CDRs/run"
         format ">>,>>>,>>9".

   assign 
      litotinv = litotinv + ttstat.invqty
      litotcdr = litotcdr + ttstat.cdrqty
      litotsub = litotsub + ttstat.subqty.
      
   create ttsum.
   assign 
      ttsum.ttday = ttstat.ttday
      ttsum.startstamp = ttstat.startstamp
      ttsum.tthour = ttstat.tthour
      ttsum.invqty = litotinv
      ttsum.cdrqty = litotcdr
      ttsum.subqty = litotsub.
end.


assign
   ldtstart = fTimeStamp2DateTime(ldstart)
   ldtend   = fTimeStamp2DateTime(ldend)
   lidur    = interval(ldtend,ldtstart,"seconds")
   lidays   = truncate(lidur / 86400,0)
   lidur    = lidur mod 86400
   lcdur    = string(lidur,"hh:mm:ss")
   lidur    = integer(entry(1,lcdur,":"))
   lidur    = lidur + lidays * 24
   lcdur    = string(lidur,"99") + ":" +
              entry(2,lcdur,":") + ":" + 
              entry(3,lcdur,":").

put stream slog unformatted
   skip(1)
   "Invoice qty: " liinvqty skip
   "Subscription qty: " lisubqty skip
   "CDR qty: " licdrqty skip
   "Duration: " lcdur
   skip(1).

for each ttsum:
   disp stream slog 
      ttsum.tthour  column-label "Hour" format ">9"
      fts2hms(ttsum.startstamp) 
         column-label "Started"
         format "x(19)"
      ttsum.invqty  column-label "Cumul. Inv.Qty"
      ttsum.subqty  column-label "Cumul. Subscr.Qty"
      ttsum.cdrqty  column-label "Cumul. CDR Qty".
end.      
                                  
/*
for each ttrun:
   disp stream slog ttrun.
end.
*/

output stream slog close.

