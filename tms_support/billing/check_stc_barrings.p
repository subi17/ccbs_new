{commpaa.i}
katun = "Qvantel".
gcBrand  = "1".
{barrfunc.i}

DEFINE VARIABLE lrBarring AS ROWID no-UNDO.
DEFINE VARIABLE lcBarring AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liPeriod AS INTEGER NO-UNDO. 
DEFINE VARIABLE liFrom AS INTEGER NO-UNDO. 
DEFINE VARIABLE lito AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldaDate AS DATE NO-UNDO INIT TODAY. 
DEFINE VARIABLE ldaLastDayOfMonth AS DATE NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE liChecked AS INTEGER NO-UNDO. 

def stream sout.

ASSIGN
   ldaDate = DATE(MONTH(ldaDate),1,YEAR(ldaDate))
   lcOutputFile = "/apps/yoigo/tms_support/billing/log/stc_barrings_" +
                  STRING(YEAR(ldaDate) * 100 + MONTH(ldaDate)) + ".txt".
def frame fReport
   lcOutputFile AT 1 FORMAT "x(60)" LABEL "Output" skip
   ldaDate      FORMAT "99.99.9999" LABEL "STC period" skip
   i            FORMAT ">>>9" LABEL "Cases found" skip
with width 80  overlay 1 column row 1 title " Incorrect STC barring check ".

UPDATE 
   lcOutputFile
   ldaDate
WITH FRAME fReport.

if lcOutputFile eq "" or lcOutputFile eq ? or ldaDate eq ? then quit.
output stream sout to value(lcOutputFile).

ASSIGN
   ldaDate = DATE(MONTH(ldaDate),1,YEAR(ldaDate))
   ldaLastDayOfMonth = fLastDayOfMOnth(ldaDate)
   liFrom = YEAR(ldaDate) * 10000 + MONTH(ldaDate) * 100 + DAY(ldaDate)
   liTo = liFrom + 1.

put stream sout unformatted 
   "MSISDN|MSSEQ|CUSTNUM|SUBS.TYPE|CURRENT_BARRING|COUNTER_TYPE|COUNTER_AMOUNT|COUNTER_LIMIT|LIMIT_EXCEEDED" skip.

FOR EACH msrequest NO-LOCK where
         msrequest.brand = gcBrand and
         msrequest.reqtype = 35 and
         msrequest.reqstatus = 2 and
         msrequest.actstamp > liFrom and
         msrequest.actstamp < liTo :
   
   liChecked = liChecked + 1.
   
   if lookup("Limits_Restricted=1",msrequest.reqcparam1) = 0 then next. 
   if msrequest.usercode ne "Cron / TMQueue" then next.
/*   if msrequest.smstext ne "TOTALTRAFFIC2" then next. */

   find first tmcounter where
      tmcounter.msseq = msrequest.msseq and
      tmcounter.tmruleseq = 3 and
      tmcounter.todate = ldaLastDayOfMonth and
      tmcounter.limitid = 2 NO-LOCK no-ERROR.
   IF NOT AVAIL tmcounter THEN  next.

   if tmcounter.limitid = 0 then next.

   find limit where
        limit.custnum = msrequest.custnum and
        limit.limittype = 1 and
        limit.tmruleseq = tmcounter.tmruleseq and
        limit.limitid = 2 and
        limit.todate >= ldaDate and
        limit.fromdate <= ldaDate NO-LOCK.
   
   find mobsub NO-LOCK where
        mobsub.msseq = msrequest.msseq no-error.
   IF NOT AVAIL mobsub then next.

   fCheckBarrStatus(MobSub.MsSeq,
                    OUTPUT lcBarring,
                    OUTPUT lrBarring).

   if tmcounter.amount < limit.limitamt then do:
  
      i = i + 1.
      disp i with frame fReport.
      pause 0.
   
      find tmrule NO-LOCK where
           tmrule.tmruleseq = tmcounter.tmruleseq.

      put stream sout unformatted 
         mobsub.cli "|"
         mobsub.msseq "|"
         mobsub.custnum "|"
         mobsub.clitype "|"
         lcBarring "|"
         tmrule.name "|"
         tmcounter.amount "|"
         limit.limitamt "|"
         tmcounter.limitid skip.

      /* find current tmcounter EXCLUSIVE-LOCK.
         tmcounter.limitid = 1.
         release tmcounter. */
   end.
end.

MESSAGE "Done:" liChecked "Errors:" i VIEW-AS ALERT-BOX.
