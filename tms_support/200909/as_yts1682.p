{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msisdn.i}

DEFINE VARIABLE lcClis AS CHARACTER NO-UNDO. 
lcclis = "622005179 622005433 622121919 622131831 622135733 622199152 622319807 622498064 622521745 622538026 622616246 622617938 622618952 622627025 622637357 622884607 622891065 633133277 633133477 633195221 633323899 633371790".
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
do i = 1 to num-entries(lcClis, " ") with frame a:
   
   find first msisdn where
      msisdn.brand = "1" and
      msisdn.cli = entry(i, lcClis," ") NO-LOCK USE-INDEX CLI.

   disp msisdn.cli msisdn.statuscode.
   if msisdn.statuscode ne 6 then next.

   fMakeMsidnHistory(RECID(msisdn)).
   assign msisdn.statuscode = 1 
          msisdn.outoperator = "". 
end.
