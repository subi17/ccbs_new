def var lcbdestlist as char no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var lcbdest as char no-undo.

def buffer btar for tariff.


lcbdestlist = "8,9,511,512,513,514,515,516,517,518,519".

for each tariff no-lock where
         tariff.brand = "1" and
         tariff.ccn = 81 and
         tariff.validto > today and
         tariff.bdest = "" and
         tariff.billcode = "10100003" and
         lookup(tariff.pricelist,"common2,contrato2") = 0:
         
   disp ccn billcode pricelist format "x(12)" validfrom validto price[1].
        
   do i = 1 to num-entries(lcbdestlist):
   
      lcbdest = entry(i,lcbdestlist).
      
      if can-find(first btar where
                        btar.brand = "1" and
                        btar.ccn   = tariff.ccn and
                        btar.bdest = lcbdest and
                        btar.pricelist = tariff.pricelist and
                        btar.validfrom = 9/1/10)
      then next. 
         
      create btar.
      buffer-copy tariff except tariffnum to btar.
      assign
         btar.tariffnum = next-value(tariff)
         btar.bdest     = lcbdest
         btar.billcode  = "10100005"
         btar.validfrom = 9/1/10
         btar.validto   = 12/31/52.
         
      j = j + 1.
   end.

   pause 0.
   disp j.
end.         
