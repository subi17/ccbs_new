{Syst/commpaa.i}
Syst.CUICommon:katun = "Qvantel".
Syst.CUICommon:gcBrand = "1".
{Func/fmakemsreq.i}

find first mobsub where mobsub.cli = "633495343" no-lock no-error.
def var liRequest as int no-undo.
def var lcResult  as char no-undo.

liRequest = fPCActionRequest(MobSub.MsSeq,
                                "MDUB2",
                                "term",
                                Func.Common:mMakeTS(),
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
      Memo.CreStamp  = Func.Common:mMakeTS()
      Memo.Brand     = Syst.CUICommon:gcBrand 
      Memo.HostTable = "MobSub" 
      Memo.KeyValue  = STRING(MobSub.MsSeq) 
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = Syst.CUICommon:katun 
      Memo.MemoTitle = "Deactivate Bundle"
      Memo.MemoText  = "Deactivated MDUB2 bundle manually, since it was activated wrongly"
      Memo.CustNum   = MobSub.CustNum
      Memo.MemoType  = "service".