{commpaa.i}
katun = "anttis".
gcBrand = "1".
{tmsconst.i}
{flimitreq.i}

input from /apps/snet/200901/Postpone+_Terminte_Actuales_v2.txt.
def stream slog.
output stream slog to /apps/snet/200901/as_ysu8.log.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsSeq AS INT NO-UNDO. 
DEFINE VARIABLE liPeriod AS INT NO-UNDO.
DEFINE VARIABLE liStatus AS INT NO-UNDO.
DEFINE VARIABLE  ldaPeriod AS DATE NO-UNDO. 
import unformatted lcLine.

repeat:
   import unformatted lcLine.
   liMsSeq = int(trim(entry(2,lcLine,"|"))).
   liStatus = int(trim(entry(5,lcLine,"|"))).
   liPeriod = int(trim(entry(6,lcLine,"|"))).
   ldaPeriod = date(1,(liPeriod MOD 100),INT(liPeriod / 100)).
   find mobsub where mobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
   IF AVAIL mobsub THEN DO:
      fCreateLimitHistory(
         mobsub.custnum,
         mobsub.msseq,
         {&LIMIT_TYPE_BILLPERM},
         liStatus,
         0,
         ldaPeriod,
         12/31/2049). 
      put stream slog unformatted mobsub.msseq "|OK " ldaperiod " " listatus skip.
   END.
   ELSE DO:
      find termmobsub WHERE termmobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
      fCreateLimitHistory(
         termmobsub.custnum,
         termmobsub.msseq,
         {&LIMIT_TYPE_BILLPERM},
         liStatus,
         0,
         ldaPeriod,
         12/31/2049). 
      put stream slog unformatted termmobsub.msseq "|OK TERMINATED " listatus skip.
   END.
end.

input close.
output stream slog close.

input from /apps/snet/200901/DenyListCustomerCleanUP20090127.txt.
output stream slog to /apps/snet/200901/as_ysu8.log2.

import unformatted lcLine.
repeat:
   import unformatted lcLine.
   liMsSeq = int(trim(entry(2,lcLine,"|"))).
   liStatus = int(trim(entry(5,lcLine,"|"))).
   find mobsub where mobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
   IF AVAIL mobsub THEN DO:
      fCreateLimitHistory(
         mobsub.custnum,
         mobsub.msseq,
         {&LIMIT_TYPE_BILLPERM},
         liStatus,
         0,
         TODAY,
         12/31/2049).
      put stream slog unformatted mobsub.msseq "|OK " listatus skip.
   END.
   ELSE DO:
      find termmobsub WHERE termmobsub.msseq = liMsSeq NO-LOCK NO-ERROR.
      fCreateLimitHistory(
         termmobsub.custnum,
         termmobsub.msseq,
         {&LIMIT_TYPE_BILLPERM},
         liStatus,
         0,
         TODAY,
         12/31/2049). 
      put stream slog unformatted termmobsub.msseq "|OK TERMINATED " listatus skip.
   END.
end.

input close.
output stream slog close.
