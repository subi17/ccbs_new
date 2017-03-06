{Syst/commpaa.i}
gcBrand = "1" .
katun = "anttis".

{Func/msisdn.i}

DEFINE VARIABLE lccli AS CHARACTER NO-UNDO init "622079047".

FIND last msisdn where msisdn.cli = lccli NO-LOCK no-error.

fMakeMsidnHistory(recid(msisdn)).
assign
   msisdn.custnum = 1001 
   msisdn.statuscode = 1
   msisdn.outoperator = "".
