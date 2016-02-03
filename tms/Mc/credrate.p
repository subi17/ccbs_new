/*------------------------------------------------------
  Module .......: credrate.p
  FUNCTION .....: display credit rating for one order
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 13.02.04
  MODIFIED .....: 11.05.04 tk input parameter changed to personid
                  03.02.06 mvi format change to credit amt
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def input parameter iiPersonId as char no-undo.

define temp-table ttCredItem
  field CredDate as date
  field Claimer  as ch
  field Amount   as de
  field Code as ch 
  index creddate is primary creddate.

def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.
def var reply       as ch                   no-undo.
def var mhlkm       as i                    no-undo.
def var item        as ch                   no-undo.
def var mhl         as ch                   no-undo.



form
    ttCredItem.CredDate 
    ttCredItem.Claimer  format "x(18)"
    ttCredItem.Amount format "->,>>>,>>9.99"
    ttCredItem.Code  format "x(3)"
    with scroll 1 8 down  row 8 centered 
    title " Credit Items " overlay frame sel.

cfc = "sel". run ufcolor. assign ccc = cfc.


find first creditrate no-lock where 
           creditrate.PersonId = iiPersonId no-error.
if not avail creditrate then return.

assign
   mhl = entry(2,crreply,"|")
   reply = entry(4,creditrate.crreply,"|")
   mhlkm = int(entry(3,creditrate.crreply,"|")).
   

do i = 0 to mhlkm - 1.
    
   item = substr(reply,532 + i * 91,91).
   create ttCredItem.
   assign
      ttCredItem.CredDate = date(substr(item,16,8))
      ttCredItem.Claimer = substr(item,40,50)
      ttCredItem.Amount = dec(substr(item,32,8))
      ttCredItem.Code = substr(item,13,3).

end.

MAIN:
repeat:

   find first ttCredItem  no-lock no-error.
   if not available ttCredItem then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(ttCredItem).
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
         find ttCredItem where recid(ttCredItem) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available ttCredItem:
            display
            ttCredItem.CredDate
            ttCredItem.Claimer
            ttCredItem.Amount
            ttCredItem.Code
            with frame sel.
            rtab[frame-line] = recid(ttCredItem).
            down with frame sel.
            find next ttCredItem no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         ehto = 3 ufkey = false.
         run ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row ttCredItem.CredDate ;(uchoose.i;) no-error with frame sel.
         color display value(ccc) ttCredItem.CredDate with frame sel.

         nap = keylabel(lastkey).

         if nap = "8" or nap = "f8" then leave MAIN. /* Return */

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find ttCredItem where recid(ttCredItem) = rtab[frame-line] no-lock.
               find prev ttCredItem no-lock no-error.
               if not available ttCredItem then do:
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
                  display ttCredItem.CredDate
                          ttCredItem.Claimer
                          ttCredItem.Amount
                          ttCredItem.Code.
                  rtab[frame-line] = recid(ttCredItem).
                  memory = recid(ttCredItem).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find ttCredItem where recid(ttCredItem) = rtab[frame-line] no-lock .
               find next ttCredItem  WHERE no-lock no-error.
               if not available ttCredItem then do:
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
                  display 
                     ttCredItem.CredDate
                     ttCredItem.Claimer
                     ttCredItem.Amount 
                     ttCredItem.Code.
                  rtab[frame-line] = recid(ttCredItem).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find ttCredItem where recid(ttCredItem) = memory no-lock no-error.
            find prev ttCredItem  WHERE no-lock no-error.
            if available ttCredItem then do:

               do i = 1 to (frame-down - 1):
                  find prev ttCredItem WHERE no-lock no-error.
                  if available ttCredItem then memory = recid(ttCredItem).
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
        if lookup(nap,"1,f1") > 0 then do:  /* CredDate */
           cfc = "puyr". run ufcolor.
           ehto = 9. run ufkey. ufkey = true.
           update CredDate with frame hayr.
           hide frame hayr no-pause.
           if CredDate ENTERED then do:
              find first ttCredItem where
                         ttCredItem.CredDate >= CredDate
              no-lock no-error.
               if not available ttCredItem then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
               end.
              /*  CredDate was found */
              assign
                memory = recid(ttCredItem)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first ttCredItem no-lock.

           memory = recid(ttCredItem).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last ttCredItem no-lock.

           memory = recid(ttCredItem).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
