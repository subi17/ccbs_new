def stream slog.
output stream slog to /apps/yoigo/tms_support/201010/fatime_other_customer.txt.

def var i as int no-undo.
def var lcsep as char no-undo init "|".
def var ldused as dec no-undo.

def temp-table ttfat no-undo
   field cli as char
   field origcust as int
   field currcust as int
   field fatgroup as char
   field amt as dec
   field used as dec
   field period as int
   field invdate as date
   index cli cli currcust fatgroup period.

for each fatime no-lock where
         fatime.invnum > 0,
   first subinvoice no-lock where
         subinvoice.invnum = fatime.invnum and
         subinvoice.subinvnum = fatime.subinvnum and
         subinvoice.msseq ne fatime.msseq,
   first invoice no-lock where
         invoice.invnum = fatime.invnum,
   first msowner no-lock use-index msseq where
         msowner.msseq = fatime.msseq and
         msowner.cli   = fatime.cli:
         
         
   if msowner.invcust = invoice.custnum then next. 
   
   find first ttfat where 
              ttfat.cli = fatime.cli and
              ttfat.currcust = invoice.custnum and
              ttfat.fatgroup = fatime.ftgrp and
              ttfat.period   = fatime.period and
              ttfat.origcust = msowner.invcust and
              ttfat.invdate = invoice.invdate no-error.
   if not available ttfat then do:
      create ttfat.
      assign 
         ttfat.cli = fatime.cli
         ttfat.currcust = invoice.custnum 
         ttfat.fatgroup = fatime.ftgrp
         ttfat.period   = fatime.period
         ttfat.origcust = msowner.invcust
         ttfat.invdate = invoice.invdate.
   end.      
    
   assign 
      ttfat.amt = ttfat.amt + fatime.amt
      ttfat.used = ttfat.used + fatime.used
      ldused = ldused + fatime.used.

   i = i + 1.
   pause 0.
   disp i invoice.invnum invoice.invdate ldused with 1 down.
   
end.

put stream slog unformatted
   "MSISDN"  lcSep
   "Original customer" lcSep
   "Customer who uses this FAT" lcSep
   "FAT type" lcSep
   "FAT amount" lcSep
   "FAT used"  lcSep
   "FAT period" lcSep
   "Invoice Date" skip.
   
for each ttfat:

   put stream slog unformatted
      ttfat.cli  lcSep
      ttfat.origcust lcSep
      ttfat.currcust lcSep
      ttfat.fatgroup lcSep
      ttfat.amt lcSep
      ttfat.used lcSep
      ttfat.period lcSep
      ttfat.invdate skip.
end.

output stream slog close.



