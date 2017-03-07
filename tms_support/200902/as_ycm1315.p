{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/fsubstermreq.i}
{Func/msisdn.i}

def stream sin.
input stream sin from /apps/snet/200902/as_ycm1315.input.

def stream slog.
output stream slog to /apps/snet/200902/as_ycm1315.log.

DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 
def buffer tmbuf for termmobsub. 


DEFINE VARIABLE liMsisdnStat AS INTEGER NO-UNDO. 
DEFINE VARIABLE liSimStat AS INTEGER NO-UNDO. 
DEFINE VARIABLE liQuarTime AS INTEGER NO-UNDO.
DEFINE VARIABLE ldaKillDate AS DATE NO-UNDO. 
DEFINE VARIABLE liTime AS INTEGER NO-UNDO. 

repeat:
   
   import stream sin unformatted lcCLi. 
   
   FIND first termmobsub where
      termmobsub.cli = lccli NO-LOCK NO-ERROR.
   
   IF NOT AVAIL termmobsub then do:
      find tmbuf where
         tmbuf.cli = termmobsub.cli and
         rowid(tmbuf) ne rowid(termmobsub) NO-LOCK.
      if tmbuf.activationdate > termmobsub.activationdate then
         find termmobsub where rowid(termmobsub) = rowid(tmbuf) NO-LOCK.
   end.

   find first msisdn where
      msisdn.brand = "1" and
      msisdn.cli = termmobsub.cli NO-LOCK use-index cli NO-eRROR.

   find first order where
      order.msseq = termmobsub.msseq and
      order.statuscode = "6" NO-LOCK NO-ERROR.
   
   FIND msrequest where
      msrequest.msseq = termmobsub.msseq and
      msrequest.reqtype = 18 and
      msrequest.reqstatus = 2 NO-LOCK NO-ERROR.

   fInitialiseValues(
      2,
      (Order.mnpstatus = 0),
      output liMsisdnStat,
      output liSimStat,
      output liQuarTime
   ).
   fSplitTS(MsRequest.ActStamp, OUTPUT ldaKillDate, OUTPUT liTime).
/*
   disp msisdn.cli msisdn.statuscode /*msisdn.validto*/ msrequest.reqiparam2 (if avail order then order.mnpstatus else -1) termmobsub.activationts format "99999999.99999" liMsisdnStat liQuarTime.
*/
  
   if msisdn.statuscode ne liMsisdnStat then do:
      put stream slog unformatted msisdn.cli "|" 
         msisdn.statuscode "|" liMsisdnStat "|" ldaKillDate skip. 
 
      fMakeMsidnHistory(recid(msisdn)).
      assign
         msisdn.statuscode = liMsisdnStat
         msisdn.portingdate = ldaKillDate. 


   end.
   else do:
      put stream slog unformatted msisdn.cli "|" 
         msisdn.statuscode "|" liMsisdnStat "|" ldaKillDate "|ok" skip. 
   end.
end.
