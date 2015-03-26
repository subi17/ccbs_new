def var ldtfrom   as date no-undo.
def var i         as int  no-undo.
def var lctoplist as char no-undo init "CONTRATODATA".

def buffer btariff for tariff.

def temp-table ttccn no-undo
     field ccn as int.
    
def stream slog.
output stream slog to /apps/snet/200903/aam_ycm1367.log append.

create ttccn.
ttccn.ccn = 30.
create ttccn.
ttccn.ccn = 51.
create ttccn.
ttccn.ccn = 81.


ldtfrom = 3/18/9.

for each ttccn,
    each tariff no-lock where
         tariff.brand = "1" and
         tariff.pricelist = "contrato4" and
         tariff.ccn = ttccn.ccn and
         tariff.bdest > "" and
         tariff.validto > ldtfrom:

   disp tariff.ccn tariff.bdest tariff.validfrom tariff.validto. 
   
   if can-find(first btariff where
                     btariff.brand = "1" and
                     btariff.pricelist = lctoplist and
                     btariff.ccn = tariff.ccn and
                     btariff.bdest = tariff.bdest and
                     btariff.validto > ldtfrom)
   then next.
   
   i = i + 1.
   disp i.

   create btariff.
   buffer-copy tariff except tariffnum pricelist to btariff.
   assign 
      btariff.tariffnum = next-value(tariff)
      btariff.pricelist = lctoplist
      btariff.validfrom = ldtfrom
      btariff.validto   = 12/31/52.
      
   put stream slog unformatted
      btariff.pricelist chr(9)
      btariff.ccn       chr(9)
      btariff.bdest     chr(9)
      btariff.validfrom chr(9)
      btariff.price[1] skip.
end.

output stream slog close.

