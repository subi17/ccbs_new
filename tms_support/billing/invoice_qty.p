def var i       as int  no-undo.
def var ldtdate as date no-undo.

pause 0.
update ldtdate format "99-99-99"
       label "Invoice Date"
       with overlay row 10 centered title " Invoice Qty " 
            side-labels frame fqty.
hide frame fqty no-pause.
       
if ldtdate = ? then return.       
 
pause 0.
disp i label "Counting .." format ">>,>>>,>>9" with 1 down overlay 
        row 10 centered frame fcount.

for each invoice no-lock use-index invdate where
         brand = "1" and
         invdate = ldtdate and
         invtype = 1:

   i = i + 1.

   if i mod 1000 = 0 then do:
      pause 0.
      disp i with frame fcount.
   end.
end.

hide frame fcount no-pause.

message i "invoices created on date" string(ldtdate,"99-99-99")
view-as alert-box title " Invoice Qty ".
