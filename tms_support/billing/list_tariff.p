def var ldafrom as date no-undo.
def var ldato   as date no-undo.
def var ldawhen as date no-undo.
def var lcfile  as char no-undo.
def var i       as int  no-undo.
def var llfirst as log  no-undo.
def var lcnum   as char no-undo.

def stream slog.

pause 0.
update 
   ldawhen 
      format "99-99-9999" 
      label "Valid On"
      help "Date when prices are valid"
      skip(1)
   "OR" skip(1)
   ldafrom 
      format "99-99-9999" 
      label "Valid From"
      help "Valid from (empty = all)"
      skip
   ldato
      format "99-99-9999" 
      label "Valid To  "
      help "Valid to (empty = all)"
      skip(1)
   lcfile 
      format "x(50)"
      label "File"
      help "File name"
with overlay row 9 centered side-labels title " TARIFFS " frame flimit.

hide frame flimit no-pause.

if ldawhen ne ? then assign
   ldafrom = ?
   ldato = ?.

if ldafrom ne ? then ldawhen = ?.

if lcfile = "" or 
   (ldawhen = ? and ldafrom = ?) or
   (ldawhen = ? and ldato = ?) 
then return.

lcnum = session:numeric-format.
session:numeric-format = "european".


output stream slog to value(lcfile).

put stream slog unformatted
   "BDest"      chr(9)
   "CCN"        chr(9)
   "PriceList"  chr(9)
   "Valid From" chr(9)
   "Valid To"   chr(9)
   "Billing Item" chr(9)
   "Min/Sec"      chr(9)
   "Unit"         chr(9)
   "Fee/Minimum Sec." chr(9)
   "TZ From"      chr(9)
   "TZ To"        chr(9)
   "Price"        chr(9)
   "Start.Charge" skip.
   
   
   
for each tariff no-lock where
         tariff.brand = "1" 
by tariff.bdest
by tariff.ccn
by tariff.pricelist:

   if ldawhen ne ? then do:
      if tariff.validfrom > ldawhen or tariff.validto < ldawhen then next.
   end.   

   else if ldafrom ne ? and tariff.validfrom ne ldafrom then do:
      if ldato ne ? and tariff.validto ne ldato then next.
   end.
   
   put stream slog unformatted 
      tariff.bdest      chr(9)
      tariff.ccn        chr(9)
      tariff.pricelist  chr(9)
      string(tariff.validfrom,"99.99.9999") chr(9)
      string(tariff.validto,"99.99.9999")   chr(9)
      tariff.billcode   chr(9)
      tariff.ratetype   chr(9)
      tariff.datatype   chr(9)
      string(tariff.discount[4],"Fee/Min.Sec") chr(9).

   llfirst = true.
         
   do i = 1 to 6:
   
      if tariff.daytype[i] = 0 then next.
      
      if not llfirst then 
      put stream slog fill(chr(9),9).
      
      put stream slog unformatted
         tariff.tzfrom[i] chr(9)
         tariff.tzto[i]   chr(9)
         trim(string(tariff.price[i],"->>>>>>>>9.99<<<<<"))  chr(9)
         trim(string(tariff.startcharge[i],"->>>>>>>9.99<<<<")) skip.
         
      llfirst = false.   
   end.
   
   if llfirst then put stream slog skip.
   
end.

session:numeric-format = lcnum.

output stream slog close.

      
