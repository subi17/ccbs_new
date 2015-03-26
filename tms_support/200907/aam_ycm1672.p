def var i as int no-undo.

def buffer bmsisdn for msisdn.

def stream slog.
output stream slog to /apps/snet/200907/aam_ycm1672.log append.

for each msisdn no-lock where
         msisdn.brand = "" and
         msisdn.cli begins "633" and
         msisdn.statuscode = 0:

   if can-find(first bmsisdn where 
                     bmsisdn.brand = "1" and
                     bmsisdn.cli = msisdn.cli)
   then next. 
   
   put stream slog unformatted msisdn.cli skip.
   
   find first bmsisdn where recid(bmsisdn) = recid(msisdn) exclusive-lock.
   bmsisdn.brand = "1".
   i = i + 1.
   
   /*
   disp statuscode validfrom validto cli
   */
   if i mod 1000 = 0 then do:
      pause 0.
      disp i with 1 down.
   end.
end.

output stream slog close.

disp i.

