/*------------------------------------------------------
  Module .......: h-obity.p 
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of billing Type
  APPLICATION ..: NN
  AUTHOR .......: jp
  CREATED ......: 28-12-99
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def shared var siirto as char.

def var ob-code     like BillType.BillType     no-undo. 
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.

form
      BillType.BillType
      BillType.BTName  format "x(30)"
    with scroll 1 11 down  row 4 centered color value(Syst.Var:cfc)
    title color value(Syst.Var:ctc) " OBI Type " overlay frame sel.

form /* SEEK Code */
    ob-code
    help "Enter Type of an Object Billing Type"
    with row 4  col 2 title color value(Syst.Var:ctc) " FIND CODE "
    color value(Syst.Var:cfc) no-labels overlay frame hayr.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. assign Syst.Var:ccc = Syst.Var:cfc.
MAIN:
repeat:

   find first BillType no-lock no-error.
   if not available BillType then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(BillType).
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
         find BillType where recid(BillType) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available BillType:
            display
            BillType.BillType
            BillType.BTName
            with frame sel.
            rtab[frame-line] = recid(BillType).
            down with frame sel.
            find next BillType no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         Syst.Var:ufk = 0 Syst.Var:ufk[1] = 35 Syst.Var:ufk[5] = 11
         Syst.Var:ufk[6] = 0 Syst.Var:ufk[8] = 8  Syst.Var:ufk[9] = 1
         siirto = ? Syst.Var:ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row BillType.BillType {Syst/uchoose.i} no-error with frame sel.
         color display value(Syst.Var:ccc) BillType.BillType with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         Syst.Var:nap = keylabel(lastkey).

         /* previous line */
         if lookup(Syst.Var:nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find BillType where recid(BillType) = rtab[frame-line] no-lock.
               find prev BillType no-lock no-error.
               if not available BillType then do:
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
                  display BillType.BillType BillType.BTName.
                  rtab[frame-line] = recid(BillType).
                  memory = recid(BillType).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(Syst.Var:nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find BillType where recid(BillType) = rtab[frame-line] no-lock .
               find next BillType no-lock no-error.
               if not available BillType then do:
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
                  display BillType.BillType BillType.BTName.
                  rtab[frame-line] = recid(BillType).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(Syst.Var:nap,"page-up,prev-page") > 0 then do with frame sel:
            find BillType where recid(BillType) = memory no-lock no-error.
            find prev BillType no-lock no-error.
            if available BillType then do:

               do i = 1 to (frame-down - 1):
                  find prev BillType no-lock no-error.
                  if available BillType then memory = recid(BillType).
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
        else if lookup(Syst.Var:nap,"page-down,next-page") > 0 then do with frame sel:
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
        if lookup(Syst.Var:nap,"1,f1") > 0 then do:  /* ob-code */
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           update ob-code with frame hayr.
           hide frame hayr no-pause.
           if ob-code ENTERED then do:
              find first BillType where BillType.BillType >= ob-code
              no-lock no-error.
               if not available BillType then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
               end.
              /*  ob-code was found */
              assign
                memory = recid(BillType)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(Syst.Var:nap,"return,enter,5,f5") > 0 then do:
           find BillType where recid(BillType) = rtab[frame-line] no-lock.
           siirto = string(BillType.BillType).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(Syst.Var:nap,"home,h") > 0 then do:
           find first BillType no-lock.
           memory = recid(BillType).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(Syst.Var:nap,"end,e") > 0 then do :
           find last BillType no-lock.
           memory = recid(BillType).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if Syst.Var:nap = "8" or Syst.Var:nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
