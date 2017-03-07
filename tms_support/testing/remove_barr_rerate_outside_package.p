{Syst/commpaa.i}
gcbrand = "1".
katun = "Qvantel".
{Func/timestamp.i}
{Func/date.i}
{Func/fdss.i}
{Func/barrfunc.i}

def var lltrue        as log   no-undo.
def var ldeTime       as dec   no-undo.
def var llrowid       as rowid no-undo.
def var lcBarrCode    as char  no-undo.
def var lcStatus      as char  no-undo.
def var ldaFromdate   as date  no-undo.
def var ldToDate      as date  no-undo.
def var ldeStamp      as dec   no-undo.

ldeStamp = fMakeTS().

def buffer bmsrequest for msrequest.

output to "/apps/yoigo/tms_support/testing/remove_barr_rerate_outside_package.txt".

assign ldaFromdate = DATE(MONTH(today),1,YEAR(today))
       ldToDate    = fLastDayOfMonth(TODAY).

EACH_MOBSUB:
for each mobsub no-lock where mobsub.paytype = false:
   FIND FIRST MServiceLimit NO-LOCK WHERE
              MServiceLimit.MsSeq    = mobsub.MsSeq AND
              MServiceLimit.dialtype = 7 and
              MServiceLimit.FromTs  <=  ldeStamp and
              MServiceLimit.EndTS   >= 99999999.99999 no-error.
   if not avail MServiceLimit then next.
   lltrue = false.
   status default mobsub.cli.
   for each mobcdr where
            mobcdr.cli = mobsub.cli      and
            mobcdr.datest >= ldaFromdate and
            mobcdr.datest <= ldToDate    and
            mobcdr.eventtype = "gprs" no-lock:
      IF Mobcdr.errorcode NE 0        THEN NEXT.
      IF MobCDR.BillCode NE "14100001" THEN NEXT.
      ldeTime  = fMake2Dt(Mobcdr.datest, Mobcdr.TimeStart).
      if ldeTime >= MServiceLimit.FromTs then do:
         lltrue = true.
         leave.
      end.
   end.

   if lltrue then do:
      IF fIsDSSActive(INPUT MobSub.CustNum, INPUT ldeStamp) THEN
         RUN Rate/cust_rate.p(MobSub.CustNum,
                         ldaFromdate,
                         ldToDate,
                         TRUE).
      ELSE
         RUN Rate/cli_rate.p(MobSub.CLI,
                        ldaFromdate,
                        ldToDate,
                        TRUE).

      lcBarrCode = fCheckBarrStatus(Mobsub.MsSeq,OUTPUT llrowid).
      IF lcBarrCode = "Y_REST" THEN DO:
         FIND FIRST bMsRequest WHERE
                    ROWID(bMsRequest) = llrowid NO-LOCK NO-ERROR.
         IF AVAIL bMsRequest AND bMsRequest.SMSTEXT BEGINS "TOTAL" THEN DO:
            RUN Mm/barrengine.p(
                MobSub.MsSeq,
                "UNY_REST",
                "5",
                katun,
                fMakeTS(),
                "",
                OUTPUT lcStatus).
            IF lcStatus = "ONC" THEN
               put unformatted mobsub.cli + "|" + mobsub.clitype + "|" + "Barred - Ongoing barring" skip.
            else
               put unformatted mobsub.cli + "|" + mobsub.clitype + "|" + "Barred - Removed and Rerated" skip.
            NEXT EACH_MOBSUB.
         end.
      end.
      put unformatted mobsub.cli + "|" + mobsub.clitype + "|" + "Not Barred - Rerated" skip.
   END. /* if lltrue then do: */

end.
output close.
