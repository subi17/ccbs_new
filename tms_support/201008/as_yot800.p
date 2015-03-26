def stream sout.
output stream sout to as_yot800.txt.


DEFINE TEMP-TABLE ttCall
   field rateccn like mobcdr.rateccn 
   field ccn like mobcdr.ccn 
   field pricelist like tariff.pricelist
   field rateplan like billtarget.rateplan 
   field billcode like mobcdr.billcode 
   field bdest like mobcdr.bdest 
   field gsmbnr like mobcdr.GsmBnr  
   field countti as int  
INDEX rateccn IS PRIMARY rateccn. 
DEFINE VARIABLE lcPrefix AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
for each mobcdr where 
   mobcdr.datest >= 8/1/2010 and
   mobcdr.datest <= 8/4/2010 and
   mobcdr.errorcode = 0 and
   mobcdr.rateccn = 81 and
   mobcdr.ccn = 81 NO-LOCK use-index date:

   FIND BillTarget WHERE
        BillTarget.CustNum    = MobCDR.CustNum AND
        BillTarget.BillTarget = MobCDR.BillTarget NO-LOCK.

   find tariff where
        tariff.brand = "1" and
        tariff.tariffnum = MobCDR.tariffnum NO-LOCK.

   lcPrefix = substring(MobCDR.gsmbnr,1,3).

   find first ttcall where
      ttcall.rateccn = MobCDR.rateccn and
      ttcall.rateplan = billtarget.rateplan and
      ttcall.ccn = MobCDR.ccn and
      ttcall.pricelist = tariff.pricelist and
      ttcall.billcode = mobcdr.billcode and
      ttcall.bdest = mobcdr.bdest and
      ttcall.gsmbnr = lcPrefix no-error.

   IF NOT AVAIL ttcall then do:
      create ttcall.
      assign
         ttcall.rateccn = MobCDR.rateccn
         ttcall.rateplan = billtarget.rateplan
         ttcall.ccn = MobCDR.ccn
         ttcall.pricelist = tariff.pricelist
         ttcall.billcode = mobcdr.billcode 
         ttcall.bdest = mobcdr.bdest 
         ttcall.gsmbnr = lcPrefix.
   end.

   ttcall.countti = ttcall.countti + 1. 
   release ttcall.

   i = i + 1.
   if i mod 1000 = 0 then do:
      disp i mobcdr.datest.
      pause 0.
   end.

end.
FOR EACH ttcall NO-LOCK:
   put stream sout unformatted
      ttcall.rateccn "|"
      ttcall.ccn "|"
      ttcall.rateplan "|"
      ttcall.pricelist "|"
      ttcall.billcode "|"
      ttcall.bdest "|"
      ttcall.GsmBnr "|"
      ttcall.countti skip.

end.
