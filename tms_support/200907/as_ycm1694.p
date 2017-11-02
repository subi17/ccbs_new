{Syst/testpaa.i}
Syst.CUICommon:katun = "anttis".
{Func/msisdn.i}

find msisdn where
   msisdn.brand = "1" and
   msisdn.cli = "622522522" NO-LOCK no-error.

IF AVAIL msisdn then do:

   fMakeMsidnHistory(recid(msisdn)).

   assign
      msisdn.statuscode = 6 
      msisdn.portingdate = today. 
end.
