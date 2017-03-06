/*------------------------------------------------------
  Module .......: H-mobsubstatus.p
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of MobSub codes
  APPLICATION ..: NN
  AUTHOR .......: JP
  CREATED ......: 10-06-99
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Mm/mobsub1.i}

def temp-table ttStatus
  field StatusCode like mobsub.msstat
  field StatusName as char format "x(20)"
  index StatusCode is primary StatusCode.

def shared var siirto as char.

def var StatusCode        like ttStatus.StatusCode     no-undo. 
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.

form
      ttStatus.StatusCode  format "zz9"
      ttStatus.StatusName  format "x(30)"
    with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " MobSub Status codes " overlay frame sel.

form /* SEEK Code */
    StatusCode
    help "Enter Code of an MobSub"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    color value(cfc) no-labels overlay frame hayr.

cfc = "sel". RUN Syst/ufcolor.p. assign ccc = cfc.

/* fill temp-table */

do i = 1 to num-entries(stnames) - 1:
   create ttStatus.
   assign 
     ttstatus.statuscode = i
     ttstatus.statusname = entry(i + 1,stnames).

end.


MAIN:
repeat:

   find first ttStatus no-lock no-error.
   if not available ttStatus then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(ttStatus).
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
         find ttStatus where recid(ttStatus) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available ttStatus:
            display
            ttStatus.StatusCode
            ttStatus.StatusName
            with frame sel.
            rtab[frame-line] = recid(ttStatus).
            down with frame sel.
            find next ttStatus no-lock no-error.
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
         choose row ttStatus.StatusCode {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) ttStatus.StatusCode with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find ttStatus where recid(ttStatus) = rtab[frame-line] no-lock.
               find prev ttStatus no-lock no-error.
               if not available ttStatus then do:
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
                  display ttStatus.StatusCode ttStatus.StatusName.
                  rtab[frame-line] = recid(ttStatus).
                  memory = recid(ttStatus).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find ttStatus where recid(ttStatus) = rtab[frame-line] no-lock .
               find next ttStatus no-lock no-error.
               if not available ttStatus then do:
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
                  display ttStatus.StatusCode ttStatus.StatusName.
                  rtab[frame-line] = recid(ttStatus).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find ttStatus where recid(ttStatus) = memory no-lock no-error.
            find prev ttStatus no-lock no-error.
            if available ttStatus then do:

               do i = 1 to (frame-down - 1):
                  find prev ttStatus no-lock no-error.
                  if available ttStatus then memory = recid(ttStatus).
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
        if lookup(nap,"1,f1") > 0 then do:  /* StatusCode */
           cfc = "puyr". RUN Syst/ufcolor.p.
           ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           update StatusCode with frame hayr.
           hide frame hayr no-pause.
           if StatusCode ENTERED then do:
              find first ttStatus where ttStatus.StatusCode >= StatusCode
              no-lock no-error.
               if not available ttStatus then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
               end.
              /*  StatusCode was found */
              assign
                memory = recid(ttStatus)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find ttStatus where recid(ttStatus) = rtab[frame-line] no-lock.
           siirto = string(ttStatus.StatusCode).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first ttStatus no-lock.
           memory = recid(ttStatus).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last ttStatus no-lock.
           memory = recid(ttStatus).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
