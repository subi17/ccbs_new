def var lcCLI   as char no-undo.
def var ldtfrom as date no-undo.
def var ldtto   as date no-undo.
def var llbilled as log  no-undo.

pause 0.
update lccli format "x(12)" label "MSISDN" colon 8 skip
       ldtfrom format "99-99-99" label "From" colon 8 skip
       ldtto   format "99-99-99" label "To" colon 8 skip
with overlay row 10 centered title " FIND MCDR " side-labels frame fcdr.

hide frame fcdr no-pause.

if lccli = "" or ldtfrom = ? or ldtto = ? then return.

for each mobcdr no-lock use-index cli where
         mobcdr.cli = lccli and
         mobcdr.datest >= ldtfrom and
         mobcdr.datest <= ldtto:

   llbilled = false.
   if mobcdr.invseq > 0 then do:
      find first invseq where invseq.invseq = mobcdr.invseq no-lock no-error.
      if available invseq and invseq.billed then llbilled = true.
   end.

   disp mobcdr.datest mobcdr.billcode mobcdr.rateccn 
        mobcdr.errorcode mobcdr.amount
        llbilled column-label "Billed".
        
end.
