def stream sread.
input stream sread from /apps/snet/200709/chk_200709_notbilled.txt.

def var lcline  as char no-undo.
def var lccli   as char no-undo.
def var ldlast  as dec  no-undo.
def var ldfirst as dec  no-undo.
def var licust  as int  no-undo.
def var i       as int  no-undo.
def var j       as int  no-undo.

repeat:
   import stream sread unformatted lcline.

   lccli = entry(1,lcline,chr(9)).


   find first invoice use-index cli where
              invoice.brand   = "1"   and
              invoice.cli     = lccli and
              invoice.invtype = 1     and
              invoice.invdate >= 9/13/7 no-lock no-error.
             

   if available invoice then next.

   i = i + 1.
   
   ldlast  = 0.
   ldfirst = 99999999.
   licust  = 0.
   
   for each msowner no-lock where
            msowner.cli = lccli 
   by msowner.tsend desc:
      ldlast = max(ldlast,msowner.tsend).
      ldfirst = min(ldfirst,msowner.tsbeg).
      
      if licust = 0 then licust = msowner.invcust.
   end.
   
   if ldfirst > 20070801 then next.

   disp i format ">>9" lccli.
   find mobsub where mobsub.cli = lccli no-lock no-error.
   if available mobsub then disp mobsub.repcodes format "x(5)".

   disp ldfirst format "99999999.99999"
        ldlast format "99999999.99999".
    
   find customer where customer.custnum = licust no-lock no-error.
   if available customer then disp customer.invgroup.
end.

disp i j.