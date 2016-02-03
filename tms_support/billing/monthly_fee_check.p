{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{Func/date.i}
{Func/cparam2.i}
{Func/timestamp.i}

DEFINE VARIABLE lcOutputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount AS INTEGER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcKey AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeActStamp AS DECIMAL NO-UNDO. 
DEFINE VARIABLE liPeriod AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldaFromDate  AS DATE NO-UNDO.
DEFINE VARIABLE ldaToDate    AS DATE NO-UNDO.
DEFINE VARIABLE ldeFromStamp AS DECIMAL NO-UNDO.
DEFINE VARIABLE ldaContractDate AS DATE NO-UNDO. 
DEFINE VARIABLE liContractTime AS INTEGER NO-UNDO. 
DEFINE VARIABLE liContractPeriod AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO format "x(10)". 
DEFINE VARIABLE lcDataContracts AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFFItemKey AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEventlogDetails AS CHARACTER NO-UNDO.
DEFINE BUFFER beventlog FOR eventlog.
DEFINE VARIABLE ldeTerminated AS DEC NO-UNDO. 

IF DAY(TODAY) = 1 THEN
   ldaToDate = fLastDayOfMOnth(TODAY - 1).
ELSE
   ldaToDate = fLastDayOfMOnth(TODAY).

ldaFromDate = DATE(MONTH(ldaToDate),1,YEAR(ldaToDate)).

ldeFromStamp = fMake2DT(ldaFromDate,0).
liPeriod = YEAR(ldaFromDate) * 100 + MONTH(ldaFromDate).

lcOutputFile = "/apps/yoigo/tms_support/billing/monthly_fee_check_" + STRING(liPeriod) + ".txt".

UPDATE 
   lcOutputFile FORMAT "x(60)" LABEL "Output" skip(1)
with width 80  overlay side-labels 1 column row 1 title " Missing monthly fee check "
frame fcontrolrep.

if lcOutputFile eq "" or lcOutputFile eq ? then quit.

disp "running.." with frame fcontrolrep.

def stream sout.
output stream sout to value(lcOutputFile).

put stream sout unformatted 
      "CUSTNUM|MSSEQ|MSISDN|CONTRACT|CONTRACT_FROM|CONTRACT_TO|MSREQUEST_CREATE_FEES|DELETE_FIXEDFEE_EVENTLOG|DELETE_FFITEM_EVENTLOG|EVENTLOG_DETAIL|TERMINATION_TIME" skip.

looppi:
FOR EACH daycampaign where
         daycampaign.brand = gcBrand AND
         daycampaign.dcevent begins "PAYTERM" NO-LOCK,
    EACH dccli where
         dccli.brand  = gcBrand and
         dccli.dcevent = daycampaign.dcevent and
         dccli.validto >= ldaFromDate NO-LOCK:
   
   i = i + 1.
   if i mod 1000 = 0 then do:
      disp i label "checked" liCount label "found" dccli.dcevent column-label "contract".
      pause 0.
   end.

   ASSIGN
      lcEventlogDetails = ""
      lcFFItemKey = ""
      ldeTerminated = 0.

   release beventlog.
   
   FIND FIRST mobsub where
              mobsub.msseq = dccli.msseq NO-LOCK no-error.
   IF NOT AVAIL mobsub then do:
      FIND FIRST termmobsub where
                 termmobsub.msseq = dccli.msseq NO-LOCK no-error.
      liCustnum = termmobsub.custnum.
      FIND FIRST msowner NO-LOCK WHERE
                 msowner.msseq = termmobsub.msseq USE-INDEX msseq.
      ldeTerminated = msowner.tsend.
   end.     
   else liCustnum = mobsub.custnum.
         
   FIND FIRST fixedfee where
              fixedfee.brand = gcBrand and
              fixedfee.custnum = liCustnum and
              fixedfee.hosttable = "mobsub" and
              fixedfee.keyvalue = string(dccli.msseq) and
              fixedfee.calcobj = dccli.dcevent and
              fixedfee.begdate >= dccli.validfrom
   NO-LOCK no-error.
   
   IF AVAIL fixedfee then do:
      FIND FIRST ffitem where
                 ffitem.FFNum = fixedfee.ffnum and
                 ffitem.billperiod = liPeriod NO-LOCK no-error.
      IF AVAIL ffitem then do:
         if (ffitem.billed and ffitem.invnum = 0) or
             (ffitem.billed eq false and ffitem.invnum > 0) then do:
            put stream sout unformatted 
                liCustnum "|" dccli.msseq "|"
             dccli.dcevent "|"
             dccli.validfrom "|"
             dccli.validto "|"
                "ERROR:FFItem billed and invoice not found (or vice versa)"
             skip.
         end.
         else if ffitem.invnum > 0 and 
          not can-find(first invoice where
                             invoice.invnum = ffitem.invnum) then do:
            put stream sout unformatted 
             liCustnum "|" dccli.msseq "|"
             dccli.dcevent "|"
             dccli.validfrom "|"
             dccli.validto "|"
             "ERROR:FFItem invoice not found" skip.
         end.

         next.
      end.
   
      if dccli.amount > 0 then do:
         FIND FIRST SingleFee NO-LOCK WHERE
                    SingleFee.Brand = gcBrand AND
                    SingleFee.Custnum = mobsub.Custnum AND
                    SingleFee.HostTable = "mobsub" AND
                    SingleFee.KeyValue = string(mobsub.msseq) AND
                    SingleFee.SourceKey = STRING(dccli.PerContractID) AND
                    SingleFee.SourceTable = "DCCLI" AND
                    SingleFee.CalcObj = "RVTERM" NO-ERROR.
         IF NOT AVAIL SingleFee then do:
            put stream sout unformatted 
                liCustnum "|" dccli.msseq "|"
                dccli.dcevent "|"
                dccli.validfrom "|"
                dccli.validto "|"
                "ERROR:Residual singlefee not found"
             skip.
         end.
      end.

   END.

   ASSIGN lcKey = "1" + CHR(255) + string(liCustnum) + CHR(255) +
          "MobSub" + CHR(255) + string(dccli.msseq)
          ldeActStamp = fmake2dt(dccli.validfrom, 0).

   IF AVAIL fixedfee THEN
      lcFFItemKey = string(fixedfee.FFNum) + CHR(255) + string(liPeriod).

   FIND FIRST msrequest where
              msrequest.msseq = dccli.msseq and
              msrequest.reqtype = 8 and
              msrequest.reqstatus = 2 and
              msrequest.reqcparam3 = dccli.dcevent and
              msrequest.actstamp >= ldeActStamp NO-LOCK.

   FIND FIRST eventlog where
              eventlog.tablename = "FixedFee" and
              eventlog.key begins lcKey and
              eventlog.action = "delete" NO-LOCK no-error.
   IF AVAIL eventlog THEN
      lcEventlogDetails = eventlog.usercode + "-" + STRING(eventlog.eventdate).
   ELSE IF lcFFItemKey > "" THEN DO:
      FIND FIRST beventlog where
                 beventlog.tablename = "FFItem" and
                 beventlog.key begins lcFFItemKey and
                 beventlog.action = "delete" NO-LOCK no-error.
      IF NOT AVAIL beventlog AND
         AVAIL fixedfee AND fixedfee.endperiod < liPeriod THEN NEXT.
      ELSE IF AVAIL beventlog THEN
         lcEventlogDetails = beventlog.usercode + "-" + STRING(beventlog.eventdate).
   END.
   
   liCount = liCount + 1.
   put stream sout unformatted 
      liCustnum "|"
      dccli.msseq "|" 
      dccli.cli "|" 
      dccli.dcevent "|" 
      dccli.validfrom "|"
      dccli.validto "|"
      (if avail msrequest then
       string(msrequest.createfees) else "N/A") "|"
      avail(eventlog) "|"
      avail(beventlog) "|"
      lcEventlogDetails "|"
      (IF ldeTerminated > 0 THEN fts2hms(ldeTerminated) ELSE "") skip.

end.

DEFINE TEMP-TABLE ttServicelimit NO-UNDO
FIELD groupcode AS char
INDEX groupcode IS PRIMARY UNIQUE groupcode. 

looppi2:
FOR EACH servicelimit NO-LOCK,
   first daycampaign NO-LOCK where
         daycampaign.brand = "1" and
         daycampaign.dcevent = servicelimit.groupcode:

   if daycampaign.feemodel eq "" then next.
   if daycampaign.dcevent begins "dss" then next.

   FIND FIRST fmitem NO-LOCK where
              fmitem.brand = "1" and
              fmitem.feemodel = daycampaign.feemodel and
              fmitem.todate > today no-error.
   if fmitem.billmethod then next.
   if fmitem.billtype eq "NF" then next. /* no fee (prepaid) */

   FIND FIRST ttServicelimit NO-LOCK where
              ttServicelimit.groupcode  = servicelimit.groupcode no-error.
   IF AVAIL ttServicelimit then next.
   create ttServicelimit.
   assign
      ttServicelimit.groupcode = daycampaign.dcevent.

   FOR EACH mservicelimit where
            mservicelimit.slseq = servicelimit.slseq and
            mservicelimit.dialtype = servicelimit.dialtype and
            mservicelimit.endts >= ldeFromStamp NO-LOCK:

      if mservicelimit.endts < mservicelimit.fromts then next.
      
      if servicelimit.groupcode = "contdata" and
         mservicelimit.fromts = 20090301.00000 then next.

      i = i + 1.
      if i mod 1000 = 0 then do:
         disp i label "checked" liCount label "found" servicelimit.groupcode column-label "contract".
         pause 0.
      end.

      ASSIGN
         lcFFItemKey = ""
         lcEventlogDetails = ""
         ldeTerminated = 0.
   
      release beventlog.
       
      fsplitts(mservicelimit.fromts, 
               output ldaContractDate, 
               output liContractTime).

      liContractPeriod = year(ldaContractDate) * 100 + month(ldaContractDate).
      
      FIND FIRST mobsub where
                 mobsub.msseq = mservicelimit.msseq NO-LOCK no-error.
      IF NOT AVAIL mobsub then do:
         FIND FIRST termmobsub where
                    termmobsub.msseq = mservicelimit.msseq NO-LOCK no-error.
         assign liCustnum = termmobsub.custnum
                lcCli = termmobsub.cli.
         FIND FIRST msowner NO-LOCK WHERE
                    msowner.msseq = termmobsub.msseq USE-INDEX msseq.
         ldeTerminated = msowner.tsend.
      end.     
      else assign liCustnum = mobsub.custnum
              lcCli = mobsub.cli.

      find first fixedfee where
                 fixedfee.brand = gcBrand and
                 fixedfee.hosttable = "mobsub" and
                 fixedfee.custnum = liCustnum and
                 fixedfee.keyvalue = string(mservicelimit.msseq) and
                 fixedfee.CalcObj = servicelimit.groupcode and
                 fixedfee.endperiod >= liContractPeriod
      NO-LOCK no-error.

      IF AVAIL fixedfee then do:
         FIND FIRST ffitem where
               ffitem.FFNum = fixedfee.ffnum and
               ffitem.billperiod = liPeriod NO-LOCK no-error.
      
         IF AVAIL ffitem then do:
            if (ffitem.billed and ffitem.invnum = 0) or
               (ffitem.billed eq false and ffitem.invnum > 0) then do:
               put stream sout unformatted 
                  liCustnum "|" mservicelimit.msseq "|"
                   "ERROR:FFItem billed and invoice not found (or vice versa)"
               skip.
            end.
            else if ffitem.invnum > 0 and 
               not can-find(first invoice where
                                  invoice.invnum = ffitem.invnum) then do:
               put stream sout unformatted 
                  liCustnum "|" mservicelimit.msseq "|"
                   "ERROR:FFItem invoice not found" skip.
            end.

            next.
         end.
      end.
      
      IF AVAIL fixedfee THEN
         lcFFItemKey = string(fixedfee.FFNum) + CHR(255) + string(liPeriod).

      FIND FIRST msrequest where
                 msrequest.msseq = mservicelimit.msseq and
                 msrequest.reqtype = 8 and
                 msrequest.reqstatus = 2 and
                 msrequest.reqcparam3 = servicelimit.groupcode and
                 msrequest.actstamp = mservicelimit.fromts NO-LOCK no-error.
   
      lcKey = "1" + CHR(255) + string(liCustnum) + CHR(255) + "MobSub" + 
              CHR(255) + string(mservicelimit.msseq). 
      
      FIND FIRST eventlog where
                 eventlog.tablename = "FixedFee" and
                 eventlog.key begins lcKey and
                 eventlog.action = "delete" NO-LOCK no-error.
      IF AVAIL eventlog THEN
         lcEventlogDetails = eventlog.usercode + "-" + STRING(eventlog.eventdate).
      ELSE IF lcFFItemKey > "" THEN DO:
         FIND FIRST beventlog where
                    beventlog.tablename = "FFItem" and
                    beventlog.key begins lcFFItemKey and
                    beventlog.action = "delete" NO-LOCK no-error.
         IF AVAIL beventlog THEN
            lcEventlogDetails = beventlog.usercode + "-" + STRING(beventlog.eventdate).
      END.
      
      put stream sout unformatted 
         liCustnum "|"
         mservicelimit.msseq "|" 
         lcCli "|"
         servicelimit.groupcode "|"
         mservicelimit.fromts "|"
         mservicelimit.endts "|"
         (if avail msrequest then
          string(msrequest.createfees) else "N/A") "|"
         avail(eventlog) "|"
         avail(beventlog) "|"
         lcEventlogDetails "|"
         (IF ldeTerminated > 0 THEN fts2hms(ldeTerminated) ELSE "") skip.
   
      liCount = liCount + 1.
   END.
END.

output stream sout close.
MESSAGE "Done," liCount "missing contracts found" VIEW-AS ALERT-BOX.
