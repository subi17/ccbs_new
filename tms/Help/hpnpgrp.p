/*------------------------------------------------------
  MODULE .......: HPNPGRP.P
  PARENT .......: APPLHELP.P
  FUNCTION .....: Help browser of PNPGroups
  APPLICATION ..: TMS
  AUTHOR .......: TK
  CREATED ......: 21.03.03
  MODIFIED .....: 26.03.03 tk find name
                  12.09.03/aam brand
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def shared var siirto as char.

def var PNPGroup    like PNPGroup.PNPGroup  no-undo.
def var Name        like PNPGroup.Name      no-undo.
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var order       as int                  no-undo INIT 1.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.

form
    PNPGroup.PNPGroup
    PNPGroup.Name  format "x(20)"
    with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " PNPGroups " overlay frame sel.

form /* SEEK code */
    PNPGroup
    help "Enter Code of a PNPGroup"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    color value(cfc) no-labels overlay frame f1.

form /* SEEK code */
    Name
    help "Enter Name of a PNPGroup"
    with row 4 col 2 title color value(ctc) " FIND NAME "
    color value(cfc) no-labels overlay frame f2.

cfc = "sel". RUN Syst/ufcolor.p. assign ccc = cfc.
MAIN:
repeat:

   find first PNPGroup WHERE PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
   if not available PNPGroup then do:
      MESSAGE "No PNPGroups available !" VIEW-AS ALERT-BOX.
      RETURN.
   end.
   else do:
      memory = recid(PNPGroup).
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
         find PNPGroup where recid(PNPGroup) = memory 
         NO-LOCK NO-ERROR.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available PNPGroup:
            display
            PNPGroup.PNPGroup
            PNPGroup.Name
            with frame sel.
            rtab[frame-line] = recid(PNPGroup).
            down with frame sel.
            IF order = 1 then find next PNPGroup WHERE 
               PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
            IF order = 2 then find next PNPGroup use-index name 
              WHERE PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 ufk[1] = 35 ufk[2] = 30 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, return:

         hide message no-pause.
         if order = 1 THEN DO:
         choose row PNPGroup.PNPGroup {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) PNPGroup.PNPGroup with frame sel.
         END.
         if order = 2 THEN DO:
         choose row PNPGroup.Name {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) PNPGroup.Name with frame sel.
         END.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         if lookup(nap,"cursor-right") > 0 THEN DO:
            order = order + 1.
            IF order = 3 THEN order = 1.
            must-print = true.
            next loop.
         END.
         if lookup(nap,"cursor-left") > 0 THEN DO:
            order = order - 1.
            IF order = 0 THEN order = 2.
            must-print = true.
            next loop.
         END.

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find PNPGroup where recid(PNPGroup) = rtab[frame-line] no-lock.
               if order = 1 then 
                  find prev PNPGroup WHERE 
                     PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
               if order = 2 then 
                  find prev PNPGroup use-index name WHERE 
                     PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
               if not available PNPGroup then do:
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
                  display PNPGroup.PNPGroup PNPGroup.Name.
                  rtab[frame-line] = recid(PNPGroup).
                  memory = recid(PNPGroup).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find PNPGroup where recid(PNPGroup) = rtab[frame-line] no-lock .
               if order = 1 then 
                  find next PNPGroup WHERE 
                     PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
               if order = 2 then 
                  find next PNPGroup use-index name WHERE 
                     PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
               if not available PNPGroup then do:
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
                  display PNPGroup.PNPGroup PNPGroup.Name.
                  rtab[frame-line] = recid(PNPGroup).
                  /* finally last line's keyvalue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find PNPGroup where recid(PNPGroup) = memory 
            NO-LOCK NO-ERROR.
            if order = 1 then 
               find prev PNPGroup WHERE 
                  PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
            if order = 2 then 
               find prev PNPGroup use-index name WHERE 
                  PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
            if available PNPGroup then do:

               do i = 1 to (frame-down - 1):
                  if order = 1 then    
                     find prev PNPGroup WHERE 
                        PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
                  if order = 2 then    
                     find prev PNPGroup use-index name WHERE 
                        PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
                  if available PNPGroup then memory = recid(PNPGroup).
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
        if lookup(nap,"1,f1") > 0 then do:  /* PNPGroup */
           cfc = "puyr". RUN Syst/ufcolor.p.
           ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           set PNPGroup with frame f1.
           hide frame f1 no-pause.
           if PNPGroup ENTERED then do:
              find first PNPGroup where PNPGroup.PNPGroup >= PNPGroup
                 AND PNPGroup.Brand = gcBrand NO-LOCK NO-ERROR.
              if not available PNPGroup then do:
                 bell.
                 message "None found !".    
                 pause 1 no-message.
                 next BROWSE.
              end.
              /*  PNPGroup was found */
              assign
                memory = recid(PNPGroup)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */
        /* Seek */

        if lookup(nap,"2,f2") > 0 then do:  /* PNPGroup */
           cfc = "puyr". RUN Syst/ufcolor.p.
           ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           set Name with frame f2.
           hide frame f2 no-pause.
           if PNPGroup ENTERED then do:
              find first PNPGroup 
              use-index name WHERE PNPGroup.Brand = gcBrand AND
                 PNPGroup.Name >= Name
              NO-LOCK NO-ERROR.
              if not available PNPGroup then do:
                 bell.
                 message "None found !".    
                 pause 1 no-message.
                 next BROWSE.
              end.
              /*  PNPGroup was found */
              assign
                memory = recid(PNPGroup)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find PNPGroup where recid(PNPGroup) = rtab[frame-line] no-lock.
           siirto = string(PNPGroup.PNPGroup).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           if order = 1 then find first PNPGroup  
              WHERE PNPGroup.Brand = gcBrand no-lock.   
           if order = 2 then find first PNPGroup use-index name 
              WHERE PNPGroup.Brand = gcBrand no-lock.
           memory = recid(PNPGroup).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           if order = 1 then find last PNPGroup 
              WHERE PNPGroup.Brand = gcBrand no-lock.
           if order = 2 then find last PNPGroup use-index name 
              WHERE PNPGroup.Brand = gcBrand no-lock.
           memory = recid(PNPGroup).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
