{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".

{Func/flimitreq.i}
{Syst/tmsconst.i}
find termmobsub where
     termmobsub.msseq = 2096335 NO-LOCK.


FIND FIRST Limit WHERE
   Limit.MsSeq = termMobSub.MsSeq AND
   Limit.LimitType = {&LIMIT_TYPE_BILLPERM} AND
   Limit.TMRuleSeq = 0 AND
   Limit.Todate >= TODAY AND
   Limit.Custnum = termMobSub.Custnum NO-LOCK NO-ERROR.
   
IF AVAIL Limit THEN 
   fSetLimit (
      ROWID(Limit),
      Limit.LimitAmt,
      FALSE, /* default value */ 
      Limit.FromDate,
      TODAY - 1). 
