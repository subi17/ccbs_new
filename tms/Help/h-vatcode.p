/*------------------------------------------------------
  Module .......: H-vatcode.P   
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of VAT codes
  APPLICATION ..: NN
  AUTHOR .......: JP
  CREATED ......: 10-6-99
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def shared var siirto as char.

def var VatCode        like VATCode.VatCode    no-undo.
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.

form
      VATCode.VatCode  
      VATCode.VCName  
    with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " VAT CODES " overlay frame sel.

form /* SEEK code */
    VatCode
    help "Enter VAT CODE"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    color value(cfc) no-labels overlay frame hayr.

cfc = "sel". run ufcolor. assign ccc = cfc.



MAIN:
repeat:

   find first VATCode no-lock no-error.
   if not available VATCode then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(VATCode).                          
      must-print = true.
   end.

pause 0.
view frame  sel.

LOOP:
   Repeat with frame sel:

print-line:
   do :
      if must-print then do:
         clear frame sel all no-pause.
         find VATCode where recid(VATCode) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available VATCode:
            display
            VATCode.VatCode
            VATCode.VCName
            with frame sel.
            rtab[frame-line] = recid(VATCode).
            down with frame sel.
            find next VATCode no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = false.
         run ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row VATCode.VatCode ;(uchoose.i;) no-error with frame sel.
         color display value(ccc) VATCode.VatCode with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find VATCode where recid(VATCode) = rtab[frame-line] no-lock.
               find prev VATCode no-lock no-error.
               if not available VATCode then do:
                  bell.
                  message "You are on 1st row !".              
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* a previous one was found */
                  scroll down.
                  do i = 11 to 2 by -1:
                     rtab[i] = rtab[i - 1].
                  end.
                  display VATCode.VatCode VATCode.VCName.
                  rtab[frame-line] = recid(VATCode).
                  memory = recid(VATCode).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find VATCode where recid(VATCode) = rtab[frame-line] no-lock .
               find next VATCode no-lock no-error.
               if not available VATCode then do:
                  bell.
                  message "You are on last row !".
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* yet another record was found */
                  scroll up.
                  do i = 1 to 10:
                     rtab[i] = rtab[i + 1].
                  end.
                  display VATCode.VatCode VATCode.VCName.
                  rtab[frame-line] = recid(VATCode).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find VATCode where recid(VATCode) = memory no-lock no-error.
            find prev VATCode no-lock no-error.
            if available VATCode then do:

               do i = 1 to (frame-down - 1):
                  find prev VATCode no-lock no-error.
                  if available VATCode then memory = recid(VATCode).
                  else i = frame-down.
               end.
               must-print = true.
               next LOOP.
            end.
            else do:
               /* this is the first data page */
               bell.
               message "This is the 1st page !".          
               pause 1 no-message.
            end.
        end. /* previous page */

        /* next page */
        else if lookup(nap,"page-down,next-page") > 0 then do with frame sel:
           if rtab[frame-down] = ? then do:
               bell.
               message "This is the last page !".
               pause 1 no-message.
           end.
           else do: /* the downmost line wasn't empty */
               memory = rtab[frame-down].
               must-print = true.
               next LOOP.
           end.
        end. /* next page */

        /* Seek */
        if lookup(nap,"1,f1") > 0 then do:  /* VatCode */
           cfc = "puyr". run ufcolor.
           ehto = 9. run ufkey. ufkey = true.
           set VatCode with frame hayr.
           hide frame hayr no-pause.
           if VatCode ENTERED then do:
              find first VATCode where VATCode.VatCode >= VatCode 
              no-lock no-error.
              if not available VATCode then do:
                 bell.
                 message "None found !".    
                 pause 1 no-message.
                 next BROWSE.
              end.
              /*  Invoicing Group  was found */
              assign
                memory = recid(VATCode)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find VATCode where recid(VATCode) = rtab[frame-line] no-lock.
           siirto = string(VATCode.VatCode).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first VATCode no-lock.
           memory = recid(VATCode).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last VATCode no-lock.
           memory = recid(VATCode).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.

