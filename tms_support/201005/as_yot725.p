{commpaa.i}
katun = "anttis".
gcBrand  = "1".
{date.i}
{msreqfunc.i}

input from Bono8_promotion.csv.

def stream slog.
output stream slog to as_yot725.log APPEND.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO. 

DEFINE VARIABLE ldeMDUBACTStart AS DECIMAL NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def buffer bmservicelimit for mservicelimit.

import unformatted lcline.
repeat:
   import unformatted lcline.
   liMsSeq = int(entry(3,lcline,';')).

   i = i + 1.
   if i <= 8 then next.
/*   if i > 8 then leave. */
   
   do trans:

   find mobsub where
      mobsub.msseq = liMsSeq NO-LOCK NO-ERROR.

   IF NOT AVAIL mobsub then do:
      find termmobsub where
           termmobsub.msseq = liMsSeq NO-LOCK.
      put stream slog unformatted lcline "|TERMINATED" skip.
      next.
   end.

   find mservicelimit where
        mservicelimit.msseq = mobsub.msseq and
        mservicelimit.slseq = 11 and
        mservicelimit.endts = 20100531.86399 EXCLUSIVE-LOCK  NO-ERROR.
   IF NOT AVAIL mservicelimit THEN DO:
      put stream slog unformatted lcline "|ERROR MDUBACT NOT FOUND" skip.
      next.
   END.

   find bmservicelimit where
        bmservicelimit.msseq = mobsub.msseq and
        bmservicelimit.slseq = 10 and
        bmservicelimit.fromts = 20100601.00000 EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL bmservicelimit THEN DO:
      put stream slog unformatted lcline "|ERROR MDUB NOT FOUND" skip.
      next.
   END.

   assign
      bmservicelimit.fromts = mservicelimit.fromts
      mservicelimit.endts = mservicelimit.fromts - 0.00001.

   find current mservicelimit no-lock.
   find current bmservicelimit no-lock.

   RUN cli_rate.p (Mobsub.cli,
                 5/1/2010,
                 5/31/2010,
                 TRUE).

   put stream slog unformatted lcLine "|OK".

   find first msrequest where
              msrequest.msseq = mobsub.msseq and
              msrequest.reqtype = 9 and
              msrequest.reqcparam2 = "term" and
              msrequest.reqcparam3 = "MDUBACT" and
              msrequest.reqstatus = 0 and
              msrequest.actstamp = 20100531.86399 NO-LOCK use-index reqcparam1
              no-error. 
   IF AVAIL msrequest then do:
      fReqStatus(4,"Bono 8 campaign May 2010 - YOT-724"). 
      put stream slog unformatted "|MDUDACT termination cancelled".
   end.
   
   put stream slog unformatted skip.

   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = fMakeTS()
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.Brand     = gcBrand
      Memo.MemoTitle = "Bono 8 campaign May 2010"
      Memo.CreUser   = "YOT-724"
      Memo.HostTable = "MobSub"
      Memo.KeyValue  = STRING(Mobsub.MsSeq)
      Memo.CustNum   = MobSub.CustNum
      Memo.Memotext  = "Bono 8 campaign May 2010 - YOT-724".

  end. /* trans */
  
end.
