{Syst/commpaa.i}
katun = "Qvantel".
gcBrand  = "1".
{Func/flimitreq.i}
{Func/date.i}
{Syst/tmsconst.i}
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE k AS INTEGER NO-UNDO. 
DEFINE VARIABLE h AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 

def stream sout.

def buffer bsinglefee for singlefee.

/*
put stream sout unformatted
   "MSSEQ|CUSTNUM|FF_ID|ORDER_ID|FF_BEGPERIOD|FFITEM_PERIOD|COMMISSION_PERIOD|ERROR" skip. */

DEFINE VARIABLE ldaPeriod AS DATE NO-UNDO formAT "99-99-9999".
DEFINE VARIABLE ldaPeriodOld AS DATE NO-UNDO. 
DEFINE VARIABLE liPeriod AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPeriodOld AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldaPeriodNext AS DATE NO-UNDO. 
DEFINE VARIABLE liPeriodNext AS INTEGER NO-UNDO. 

ldaPeriod = today.

update ldaPeriod label "Billing period" with frame b title " Ultimate FF check ".
if ldaPeriod eq ? then return.
if ldaPeriod < 3/1/2014 then return.
   
liPeriod = year(ldaPeriod) * 100 + month(ldaPeriod).
ldaPeriodOld = date(month(ldaPeriod),1,year(ldaPeriod)) - 1.
ldaPeriodOld = date(month(ldaPeriodOld),1,year(ldaPeriodOld)).
liPeriodOld = year(ldaPeriodOld) * 100 + month(ldaPeriodOld).
ldaPeriodNext = flastdayofmonth(ldaPeriod) + 1.
liPeriodNext = year(ldaPeriodNext) * 100 + month(ldaPeriodNext).

output stream sout to value("/apps/yoigo/tms_support/billing/log/ultimate_report_fixedfee_" + 
   string(liPeriod) + ".txt").

put stream sout unformatted "ORDER_ID|CUSTNUM|MSISDN|MSSEQ|FF_ID|SUBSRIPTION_ACTIVE|FEE_AMT|COMMISSION_AMT|PENALTY_AMT|BILLED_MONTHLY_FEE|FF_BEG_PERIOD|FFITEM_PERIOD|ACC|BILLED|BILLING_PERMISSION" skip.

fOR EACH fixedfee NO-LOCK where
         fixedfee.ffnum > 6127213 and
         fixedfee.billcode eq "PAYTERM":

   if fixedfee.begdate < ldaPeriodOld then next.
   if fixedfee.begdate > flastdayofmonth(ldaPeriod) then next.

   if fixedfee.financedresult ne "00" then next.

   release bsinglefee.

   FIND FIRST ffitem of fixedfee NO-LOCK no-error.
   
   if avail ffitem and ffitem.billperiod < liPeriod then next.
   if fixedfee.begperiod < liPeriodOld then next.

  /* 
   if avail ffitem and ffitem.billperiod = 201305 and
            ffitem.billed = true and
            ffitem.invnum < 68329482 then do:
      FIND FIRST invoice NO-LOCK where
                 invoice.invnum = ffitem.invnum NO-ERROR.
      IF AVAIL invoice and invoice.invtype eq 1 and
               invoice.invdate < 7/1/2013 then next. 
   end.
   */
   i = i + 1.
   if i mod 100 = 0 then do:
      disp fixedfee.begdate i column-label "checked"
         j column-label "errors"  with frame a.
      pause 0.
   end.
   
   FIND FIRST mobsub NO-LOCK where
              mobsub.msseq = int(fixedfee.keyvalue) NO-ERROR.
   IF AVAIL mobsub then lcCli = mobsub.cli.
   else do:
   FIND FIRST termmobsub NO-LOCK where
             termmobsub.msseq = int(fixedfee.keyvalue) NO-ERROR.
      IF NOT AVAIL termmobsub then MESSAGE "wtf" VIEW-AS ALERT-BOX.
      lcCli = termmobsub.cli.
   end.

   
   FIND FIRST msrequest NO-LOCK where
              msrequest.msseq = int(fixedfee.keyvalue) and
              msrequest.reqtype = 10 and
              msrequest.reqstatus ne 4 and
              msrequest.actstamp  = (liPeriodNext * 100 + 1) no-error.

   fGetLimit(fixedfee.custnum, int(fixedfee.keyvalue), {&LIMIT_TYPE_BILLPERM}, 0, 0, TODAY).
   
   FIND FIRST singlefee NO-LOCK where
              singlefee.brand = "1" and
              singlefee.hosttable = "mobsub" and
              singlefee.keyvalue = fixedfee.keyvalue and
              singlefee.calcobj =  fixedfee.calcobj and
              singlefee.sourcekey eq string(fixedfee.ffnum) and
             (singlefee.billcode = "PAYTERMCG1E" OR
              singlefee.billcode = "PAYTERMCGBS") NO-ERROR.

   if avail singlefee and 
            singlefee.billperiod = liPeriodOld and
            singlefee.billed = true then next.
   
   if fixedfee.orderid eq 0 then do:
      put stream sout unformatted
         fixedfee.orderid "|"
         fixedfee.custnum "|"
         lcCLi "|"
         fixedfee.keyvalue "|"
         fixedfee.ffnum "|" 
         avail(mobsub) "|"
         (if avail(ffitem) then string(ffitem.amt) ELSE "N/A") "|"
         (if avail(singlefee) then string(singlefee.amt) else "N/A") "|" 
         fixedfee.begperiod "|"
         (if avail ffitem then ffitem.billperiod else "N/A") "|"
         (avail msrequest) "|"
         (if avail limit then limit.limitamt else 0) "|"
         "ERROR:order id not mapped" skip.
         j = j + 1.
      next.
   end.
      
   FIND FIRST bsinglefee NO-LOCK where
              bsinglefee.brand = "1" and
              bsinglefee.hosttable = "mobsub" and
              bsinglefee.keyvalue = fixedfee.keyvalue and
              bsinglefee.billcode = "PAYTERMEND"  and
              bsinglefee.sourcetable = "fixedfee" and
              bsinglefee.sourcekey = string(fixedfee.ffnum) NO-ERROR.
   
   IF NOT AVAIL singlefee and avail(ffitem) then do:
   
      put stream sout unformatted
         fixedfee.orderid "|"
         fixedfee.custnum "|"
         lcCLi "|"
         fixedfee.keyvalue "|"
         fixedfee.ffnum "|" 
         avail(mobsub) "|"
         (if avail(ffitem) then string(ffitem.amt) ELSE "N/A") "|"
         (if avail(singlefee) then string(singlefee.amt) else "N/A") "|" 
         (if avail(bsinglefee) then string(bsinglefee.amt) else "N/A") "|" 
         (if avail(ffitem) then string(ffitem.billed) else "N/A") "|"
         fixedfee.begperiod "|"
         ffitem.billperiod "|"
         (avail msrequest) "|"
         (if avail limit then limit.limitamt else 0) "|"
         "ERROR:commission fee missing" skip.
         j = j + 1.
      next.
   end.
   if avail singlefee and avail ffitem and
   singlefee.billperiod ne ffitem.billperiod then do:
      put stream sout unformatted
         fixedfee.orderid "|"
         fixedfee.custnum "|"
         lcCLi "|"
         fixedfee.keyvalue "|"
         fixedfee.ffnum "|" 
         avail(mobsub) "|"
         (if avail(ffitem) then string(ffitem.amt) ELSE "N/A") "|"
         (if avail(singlefee) then string(singlefee.amt) else "N/A") "|" 
         (if avail(bsinglefee) then string(bsinglefee.amt) else "N/A") "|" 
         (if avail(ffitem) then string(ffitem.billed) else "N/A") "|"
         fixedfee.begperiod "|"
         ffitem.billperiod "|"
         (avail msrequest) "|"
         (if avail limit then limit.limitamt else 0) "|"
         "ERROR:commission fee period differs" skip.
         j = j + 1.
      next.
   end.
   else if not avail(ffitem) and
      avail singlefee then do:
      put stream sout unformatted
         fixedfee.orderid "|"
         fixedfee.custnum "|"
         lcCLi "|"
         fixedfee.keyvalue "|"
         fixedfee.ffnum "|" 
         avail(mobsub) "|"
         (if avail(ffitem) then string(ffitem.amt) ELSE "N/A") "|"
         (if avail(singlefee) then string(singlefee.amt) else "N/A") "|" 
         (if avail(bsinglefee) then string(bsinglefee.amt) else "N/A") "|" 
         (if avail(ffitem) then string(ffitem.billed) else "N/A") "|"
         fixedfee.begperiod "|"
         (if avail ffitem then string(ffitem.billperiod) else "N/A") "|"
         (avail msrequest) "|"
         (if avail limit then limit.limitamt else 0) "|"
         "ERROR:commission fee without monthly fee" skip.
         j = j + 1.
      next.
   
   end.
   
   if fixedfee.endperiod < liPeriodNext and
      fixedfee.endperiod >= fixedfee.begperiod and 
      not avail bsinglefee then do:
   
      put stream sout unformatted
         fixedfee.orderid "|"
         fixedfee.custnum "|"
         lcCLi "|"
         fixedfee.keyvalue "|"
         fixedfee.ffnum "|" 
         avail(mobsub) "|"
         (if avail(ffitem) then string(ffitem.amt) ELSE "N/A") "|"
         (if avail(singlefee) then string(singlefee.amt) else "N/A") "|" 
         (if avail(bsinglefee) then string(bsinglefee.amt) else "N/A") "|" 
         (if avail(ffitem) then string(ffitem.billed) else "N/A") "|"
         fixedfee.begperiod "|"
         (if avail ffitem then string(ffitem.billperiod) else "N/A") "|"
         (avail msrequest) "|"
         (if avail limit then limit.limitamt else 0) "|"
         "WARNING:No penalty fee"
         skip.
/*      j = j + 1. */
   next.
   end.
      
   put stream sout unformatted
      fixedfee.orderid "|"
      fixedfee.custnum "|"
      lcCLi "|"
      fixedfee.keyvalue "|"
      fixedfee.ffnum "|" 
      avail(mobsub) "|"
      (if avail(ffitem) then string(ffitem.amt) ELSE "N/A") "|"
      (if avail(singlefee) then string(singlefee.amt) else "N/A") "|" 
      (if avail(bsinglefee) then string(bsinglefee.amt) else "N/A") "|" 
         (if avail(ffitem) then string(ffitem.billed) else "N/A") "|"
      fixedfee.begperiod "|"
      (if avail ffitem then string(ffitem.billperiod) else "N/A") "|"
      (avail msrequest) "|"
      (if avail limit then limit.limitamt else 0) "|"
      skip.

end.
