/*------------------------------------------------------
  Module .......: H-SIMART.P
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of SIM card's article codes
  APPLICATION ..: NN
  AUTHOR .......: JP
  CREATED ......: 10-06-99
  MODIFIED .....: 15.09.03 jp Brand 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def shared var siirto as char.

def var SimArt        like SimArt.SimArt   no-undo.
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.

form
      SimArt.SimArt
      SimArt.SAName  format "x(20)"
    with scroll 1 11 down  row 4 centered color value(Syst.CUICommon:cfc)
    title color value(Syst.CUICommon:ctc) " SIM-cards " overlay frame sel.

form /* SEEK Code */
    SimArt
    help "Enter article code of an SIM-card"
    with row 4 col 2 title color value(Syst.CUICommon:ctc) " FIND CODE  "
    color value(Syst.CUICommon:cfc) no-labels overlay frame hayr.

Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. assign Syst.CUICommon:ccc = Syst.CUICommon:cfc.
MAIN:
repeat:

   find first SimArt WHERE SimArt.Brand = Syst.CUICommon:gcBrand no-lock no-error.
   if not available SimArt then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(SimArt).
      must-print = true.
   end.

pause 0.
view frame sel.

LOOP:
   Repeat with frame sel:

print-line:
   do :
      if must-print then do:
         clear frame sel all no-pause.
         find SimArt where recid(SimArt) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available SimArt:
            display
            SimArt.SimArt
            SimArt.SAName
            with frame sel.
            rtab[frame-line] = recid(SimArt).
            down with frame sel.
            find next SimArt WHERE SimArt.Brand = Syst.CUICommon:gcBrand 
            no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         Syst.CUICommon:ufk = 0 Syst.CUICommon:ufk[1] = 35 Syst.CUICommon:ufk[5] = 11
         Syst.CUICommon:ufk[6] = 0 Syst.CUICommon:ufk[8] = 8  Syst.CUICommon:ufk[9] = 1
         siirto = ? Syst.CUICommon:ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row SimArt.SimArt {Syst/uchoose.i} no-error with frame sel.
         color display value(Syst.CUICommon:ccc) SimArt.SimArt with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         Syst.CUICommon:nap = keylabel(lastkey).

         /* previous line */
         if lookup(Syst.CUICommon:nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find SimArt where recid(SimArt) = rtab[frame-line] no-lock.
               find prev SimArt WHERE 
                         SimArt.Brand = Syst.CUICommon:gcBrand no-lock no-error.
               if not available SimArt then do:
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
                  display SimArt.SimArt SimArt.SAName.
                  rtab[frame-line] = recid(SimArt).
                  memory = recid(SimArt).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(Syst.CUICommon:nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find SimArt where recid(SimArt) = rtab[frame-line] no-lock .
               find next SimArt WHERE 
                         SimArt.Brand = Syst.CUICommon:gcBrand no-lock no-error.
               if not available SimArt then do:
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
                  display SimArt.SimArt SimArt.SAName.
                  rtab[frame-line] = recid(SimArt).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(Syst.CUICommon:nap,"page-up,prev-page") > 0 then do with frame sel:
            find SimArt where recid(SimArt) = memory no-lock no-error.
            find prev SimArt  WHERE
                      SimArt.Brand = Syst.CUICommon:gcBrand no-lock no-error.
            if available SimArt then do:

               do i = 1 to (frame-down - 1):
                  find prev SimArt no-lock no-error.
                  if available SimArt then memory = recid(SimArt).
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
        else if lookup(Syst.CUICommon:nap,"page-down,next-page") > 0 then do with frame sel:
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
        if lookup(Syst.CUICommon:nap,"1,f1") > 0 then do:  /* SimArt */
           Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
           Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           set SimArt with frame hayr.
           hide frame hayr no-pause.
           if SimArt ENTERED then do:
              find first SimArt where 
                         SimArt.Brand   = Syst.CUICommon:gcBrand AND 
                         SimArt.SimArt >= SimArt
              no-lock no-error.
              if not available SimArt then do:
                 bell.
                 message "None found !".    
                 pause 1 no-message.
                 next BROWSE.
              end.
              /*  SimArt was found */
              assign
                memory = recid(SimArt)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(Syst.CUICommon:nap,"return,enter,5,f5") > 0 then do:
           find SimArt where recid(SimArt) = rtab[frame-line] no-lock.
           siirto = string(SimArt.SimArt).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(Syst.CUICommon:nap,"home,h") > 0 then do:
           find first SimArt no-lock  WHERE
                      SimArt.Brand = Syst.CUICommon:gcBrand.
           memory = recid(SimArt).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(Syst.CUICommon:nap,"end,e") > 0 then do :
           find last SimArt no-lock  WHERE
                     SimArt.Brand = Syst.CUICommon:gcBrand.
           memory = recid(SimArt).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if Syst.CUICommon:nap = "8" or Syst.CUICommon:nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
