ff/* Script for fixing "Subscription deleted by error as not outporting option" 
   issues (e.g. YCM-1453 & YCM-1533) */

{Syst/commali.i}
{Func/fsubstermreq.i}
{Func/msisdn.i}

DEFINE INPUT PARAMETER pcInput AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcOutput AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER ilSimulate AS LOGICAL NO-UNDO. 

def stream sin.
input stream sin from VALUE(pcInput).

def stream slog.
output stream slog to VALUE(pcOutput).

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

   FOR EACH tmbuf WHERE 
            tmbuf.cli = termmobsub.cli NO-LOCK: 
      
      if tmbuf.activationdate > termmobsub.activationdate then
         find termmobsub where rowid(termmobsub) = rowid(tmbuf) NO-LOCK.

   end.

   find first msisdn where
      msisdn.brand = "1" and
      msisdn.cli = termmobsub.cli NO-LOCK use-index cli NO-eRROR.
   
   find first order where
       order.msseq = termmobsub.msseq and
       order.statuscode = "6" and
       order.ordertype ne 2 NO-LOCK NO-ERROR.


   FIND msrequest where
      msrequest.msseq = termmobsub.msseq and
      msrequest.reqtype = 18 and
      msrequest.reqstatus = 2 NO-LOCK NO-ERROR.

   fInitialiseValues(
      2,
      (termmobsub.CLI BEGINS "622" OR termmobsub.CLI BEGINS "633"),
      false,
      output liMsisdnStat,
      output liSimStat,
      output liQuarTime
   ).

   fSplitTS(MsRequest.ActStamp, OUTPUT ldaKillDate, OUTPUT liTime).
     
   find mobsub where
    mobsub.cli  = msisdn.cli NO-LOCK NO-eRROR.
   IF AVAIL mobsub then do:
     put stream slog unformatted msisdn.cli "|" 
         msisdn.statuscode "|" liMsisdnStat "|" ldaKillDate "|ERROR: subscription exists!" skip. 
     next.
  
   end.

   if msisdn.statuscode ne liMsisdnStat then do:
      
      put stream slog unformatted msisdn.cli "|" 
         msisdn.statuscode "|" liMsisdnStat "|" ldaKillDate skip. 
      if not ilSimulate then do:
         fMakeMsidnHistory(recid(msisdn)).
         assign
            msisdn.statuscode = liMsisdnStat
            msisdn.portingdate = ldaKillDate. 
      end.

   end.
   else do:
      put stream slog unformatted msisdn.cli "|" 
         msisdn.statuscode "|" liMsisdnStat "|" ldaKillDate "|ok" skip. 
   end.
end.
