{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/msisdn.i}

FIND FIRST msisdn where
   msisdn.brand = gcBrand and
   msisdn.cli = "622567039" EXCLUSIVE-LOCK NO-ERROR.
FIND FIRST termmobsub where termmobsub.cli = "622567039".

fMakeMsidnHistory(recid(msisdn)).

assign
  msisdn.statuscode = 6
  msisdn.validto = 99999999.99999
  msisdn.outoperator = "724015".
  msisdn.portingdate = 10/22/2008.
create memo.

ASSIGN
   memo.CreStamp  = fmakeTS()
   memo.MemoSeq   = NEXT-VALUE(MemoSeq)
   Memo.Custnum   = TERMMobSub.CustNum
   memo.HostTable = "MobSub"
   memo.KeyValue  = STRING(TermMobsub.CustNum)
   memo.CreUser   = "cst002" 
   memo.MemoTitle = "OUTPORTED to " + "724015"
   Memo.memotext  = "Number:" + TermMobSub.CLI.

