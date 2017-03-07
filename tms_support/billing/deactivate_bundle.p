{Func/date.i}
{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{Func/timestamp.i}
{Func/fmakemsreq.i}

find first mobsub where mobsub.cli = "633495343" no-lock no-error.
def var liRequest as int no-undo.
def var lcResult  as char no-undo.

liRequest = fPCActionRequest(MobSub.MsSeq,
                                "MDUB2",
                                "term",
                                fmakeTS(),
                                FALSE,    /* fees */
                                "4",
                                "",   /* creator */
                                0,    /* no father request */
                                FALSE,
                                "",
                                OUTPUT lcResult).
message liRequest skip lcResult view-as alert-box.

CREATE Memo.
   ASSIGN
      Memo.CreStamp  = fmakeTS()
      Memo.Brand     = gcBrand 
      Memo.HostTable = "MobSub" 
      Memo.KeyValue  = STRING(MobSub.MsSeq) 
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = katun 
      Memo.MemoTitle = "Deactivate Bundle"
      Memo.MemoText  = "Deactivated MDUB2 bundle manually, since it was activated wrongly"
      Memo.CustNum   = MobSub.CustNum
      Memo.MemoType  = "service".