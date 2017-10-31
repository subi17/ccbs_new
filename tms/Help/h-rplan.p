/*------------------------------------------------------
  Module .......: H-RPLAN.P
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of Rating plan
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 16.09.02
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def shared var siirto as char.

def var RatePlan        like RatePlan.RatePlan   no-undo.
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.

form
      RatePlan.RatePlan
      RatePlan.RPName  format "x(30)"
    with scroll 1 11 down  row 4 centered color value(Syst.CUICommon:cfc)
    title color value(Syst.CUICommon:ctc) " Rating Plans " overlay frame sel.

form /* SEEK Code */
    RatePlan
    help "Enter Code of a Rating Plan"
    with row 4 col 2 title color value(Syst.CUICommon:ctc) " FIND CODE "
    color value(Syst.CUICommon:cfc) no-labels overlay frame hayr.

Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. assign Syst.CUICommon:ccc = Syst.CUICommon:cfc.
MAIN:
repeat:

   find first RatePlan WHERE RAteplan.Brand = gcBrand  no-lock no-error.
   if not available RatePlan then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(RatePlan).
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
         find RatePlan where recid(RatePlan) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available RatePlan:
            display
            RatePlan.RatePlan
            RatePlan.RPName
            with frame sel.
            rtab[frame-line] = recid(RatePlan).
            down with frame sel.
            find next RatePlan  WHERE RAteplan.Brand = gcBrand no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row RatePlan.RatePlan {Syst/uchoose.i} no-error with frame sel.
         color display value(Syst.CUICommon:ccc) RatePlan.RatePlan with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find RatePlan where recid(RatePlan) = rtab[frame-line] no-lock.
               find prev RatePlan  WHERE RAteplan.Brand = gcBrand 
               no-lock no-error.
               if not available RatePlan then do:
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
                  display RatePlan.RatePlan RatePlan.RPName.
                  rtab[frame-line] = recid(RatePlan).
                  memory = recid(RatePlan).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find RatePlan where recid(RatePlan) = rtab[frame-line] no-lock .
               find next RatePlan  WHERE RAteplan.Brand = gcBrand
               no-lock no-error.
               if not available RatePlan then do:
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
                  display RatePlan.RatePlan RatePlan.RPName.
                  rtab[frame-line] = recid(RatePlan).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find RatePlan where recid(RatePlan) = memory no-lock no-error.
            find prev RatePlan  WHERE RAteplan.Brand = gcBrand
            no-lock no-error.
            if available RatePlan then do:

               do i = 1 to (frame-down - 1):
                  find prev RatePlan  WHERE RAteplan.Brand = gcBrand
                  no-lock no-error.
                  if available RatePlan then memory = recid(RatePlan).
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
        if lookup(nap,"1,f1") > 0 then do:  /* RatePlan */
           Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
           ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           set RatePlan with frame hayr.
           hide frame hayr no-pause.
           if RatePlan ENTERED then do:
              find first RatePlan where 
                         RatePlan.Brand     = gcBrand AND 
                         RatePlan.RatePlan >= RatePlan
              no-lock no-error.
               if not available RatePlan then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
               end.
              /*  RatePlan  was found */
              assign
                memory = recid(RatePlan)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find RatePlan where recid(RatePlan) = rtab[frame-line] no-lock.
           siirto = string(RatePlan.RatePlan).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first RatePlan  WHERE RAteplan.Brand = gcBrand no-lock.
           memory = recid(RatePlan).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last RatePlan  WHERE RAteplan.Brand = gcBrand no-lock.
           memory = recid(RatePlan).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
