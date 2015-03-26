{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msisdn.i}

input from as_yot633.input.

DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO. 

repeat:
   import unformatted lcCli.
   
   find first msisdn where
      msisdn.brand = "1" and
      msisdn.cli = lcCLI NO-LOCK USE-INDEX CLI.

   if msisdn.pos ne "vip" or msisdn.statuscode ne 0 or
      msisdn.validto ne 99999999.99999 then do:
      disp msisdn.pos msisdn.statuscode msisdn.validto.
      next.
   end.

   do trans:
      fMakeMsidnHistory(RECID(msisdn)).
      assign msisdn.statuscode = 1. 
      release msisdn.
   end.
end.
