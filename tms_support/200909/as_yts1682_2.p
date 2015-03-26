{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msisdn.i}

DEFINE VARIABLE lcClis AS CHARACTER NO-UNDO. 
lcclis = "622005433 622498064 622521745 622538026".
/*
def stream slog.
output stream slog to /apps/snet/200909/as_yts1682_2.d.
*/
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
do i = 1 to num-entries(lcClis, " ") with frame a:
   
   find first msisdn where
      msisdn.brand = "1" and
      msisdn.cli = entry(i, lcClis," ") EXCLUSIVE-LOCK USE-INDEX CLI.
/*
   if msisdn.statuscode ne 1 then next.
   export stream slog msisdn.
   delete msisdn.
*/
   if msisdn.statuscode ne 6 then next.
   msisdn.validto = 99999999.99999.
   disp msisdn.cli msisdn.statuscode msisdn.validto.

end.
