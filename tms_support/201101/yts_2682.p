DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def buffer bmsrequest for msrequest.
def stream sout.
output stream sout to yts_2682.log.
{date.i}

{commpaa.i}
gcBrand = "1".
katun = "anttis".

put stream sout unformatted "MSISDN|old clitype|new clitype|Request creation time|request handling time|contract" skip.

FOR EACH msrequest where
         msrequest.brand = "1" and
         msrequest.reqtype = 8 and
         msrequest.reqstatus = 2 and
         msrequest.actstamp >= 20110201 and
         msrequest.actstamp < 20110203 and
         msrequest.origrequest > 0 NO-LOCK:

   find bmsrequest where
        bmsrequest.msrequest = msrequest.origrequest and
        bmsrequest.reqtype = 0 NO-LOCK no-error.
   IF NOT AVAIL bmsrequest then next.
   i = i + 1.

   if lookup(msrequest.reqcparam3,"yoigoyoigo,gprs") > 0 then next.

   find first msowner where
      msowner.brand = "1" and
      msowner.cli = msrequest.cli NO-LOCK.

   if index(bmsrequest.reqcparam1,"contrd") > 0 and
      index(bmsrequest.reqcparam1,"contrd") > 0 then next.

   find servicelimit where 
        servicelimit.slcode = msrequest.reqcparam3 NO-LOCK.

   find mservicelimit where
        mservicelimit.msseq = bmsrequest.msseq and
        mservicelimit.slseq = servicelimit.slseq and
        mservicelimit.endts > fMakeTS() NO-LOCK.
     
   put stream sout unformatted
      msrequest.cli "|" 
      bmsrequest.reqcparam1 "|"
      bmsrequest.reqcparam2 "|"
      fts2hms(msrequest.actstamp) "|"
      fts2hms(msrequest.donestamp) "|"
      msrequest.reqcparam3 "|"
      fts2hms(mservicelimit.fromts) skip.

   find current mservicelimit EXCLUSIVE-LOCK.
   mservicelimit.fromts = msrequest.actstamp.
   find current mservicelimit NO-LOCK.

   run cli_rate.p (msrequest.cli,
                   2/1/11,
                   2/28/11,
                   true). 
end.

disp i.
