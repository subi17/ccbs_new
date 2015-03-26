/* ----------------------------------------------------------------------
  MODULE .......: msisdn_release.p 
  TASK .........: Set msisdns available for sales after quarantine time has
                  passed. (YBR-50 & YNC-38)
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 21.01.09
  Version ......: xfera
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{tmsconst.i}
{msisdn.i}
DEFINE VARIABLE secs AS INTEGER NO-UNDO.
DEFINE VARIABLE ldDate AS DATE NO-UNDO.
DEFINE VARIABLE lcPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE liQuarTime AS INTEGER NO-UNDO. 

lcPath = {&PATH_MSISDN_RELEASE}.
lcFileName = "msisdn_release_" +
             STRING(YEAR(TODAY) * 10000 + MONTH(TODAY) + DAY(TODAY)) + "_" +
             STRING(MTIME) + ".txt".

def stream sout.
output stream sout to value(lcPath + lcFileName).

def buffer msisdnBuf for msisdn.

put stream sout unformatted "MSISDN|Days in quarantine" skip.

FOR EACH msisdnBuf NO-LOCK WHERE
         msisdnBuf.brand = gcBrand
   USE-INDEX CLI BREAK BY msisdnBuf.cli:

   IF FIRST-OF(msisdnBuf.cli) AND 
      (msisdnBuf.statuscode = 4 OR msisdnBuf.statuscode = 15) THEN DO:

      fSplitTS(msisdnBuf.validFrom, output ldDate, output secs).

      IF msisdnBuf.StatusCode = 4 THEN liQuarTime = 90. ELSE liQuarTime = 10.

      IF TODAY - ldDate >= liQuarTime THEN DO:

         put stream sout unformatted
            msisdnBuf.cli "|"  msisdnBuf.validfrom "|" TODAY - ldDate SKIP.

         fMakeMsidnHistory(recid(msisdnBuf)).

         assign
            msisdn.custnum = 0
            msisdn.statuscode = 1
            msisdn.orderid = 0
            msisdn.msseq   = 0
            msisdn.outoperator = "". 

         RELEASE Msisdn.
      END.
   END.
END.

output stream sout close.
