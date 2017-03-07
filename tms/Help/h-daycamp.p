/*------------------------------------------------------
  Module .......: h-daycamp.p
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of DayCampaign 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 12.04.06
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def shared var siirto as char.

DEF VAR lcEvent     as char                 no-undo. 
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.
DEF VAR lcRestricted AS CHAR NO-UNDO.
DEF VAR lcDCTypes    AS CHAR NO-UNDO.

form
    DayCampaign.DCEvent    COLUMN-LABEL "Contract"
    DayCampaign.DCName     COLUMN-LABEL "Name"
    with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " Periodical Contracts " overlay frame sel.

form /* SEEK Code */
    lcEvent FORMAT "X(12)"
    help "Enter contract"
    with row 4 col 2 title color value(ctc) " FIND CONTRACT"
    color value(cfc) no-labels overlay frame hayr.

cfc = "sel". RUN Syst/ufcolor.p. assign ccc = cfc.

IF gcHelpParam > "" THEN DO:
   IF ENTRY(1,gcHelpParam,":") = "Restricted" AND
      NUM-ENTRIES(gcHelpParam,":") > 1 THEN 
      lcRestricted = ENTRY(2,gcHelpParam,":").
   ELSE IF ENTRY(1,gcHelpParam,":") = "DCType" AND
      NUM-ENTRIES(gcHelpParam,":") > 1 THEN 
      lcDCTypes = ENTRY(2,gcHelpParam,":").

   gcHelpParam = "".
END.

MAIN:
repeat:

   RUN local-find-first. 
   if not available DayCampaign then do:
      must-print = false.
      must-add = false.
   end.
   else do:
      memory = recid(DayCampaign).
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
         find DayCampaign where recid(DayCampaign) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available DayCampaign:
         
            run local-disp-row.

            rtab[frame-line] = recid(DayCampaign).
            down with frame sel.
            RUN local-find-next.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 
         ufk[1] = 1045 WHEN lcRestricted = "" AND lcDCTypes = ""
         ufk[5] = 11
         ufk[6] = 0  ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

      BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row DayCampaign.DCEvent {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) DayCampaign.DCEvent with frame sel.

         nap = keylabel(lastkey).

         if frame-value = "" and rtab[frame-line] = ? and
            lookup(nap,"8,f8") = 0
         then next.

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find DayCampaign where recid(DayCampaign) = rtab[frame-line] 
                    no-lock.
               RUN local-find-prev.
               if not available DayCampaign then do:
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
      
                  run local-disp-row.

                  rtab[frame-line] = recid(DayCampaign).
                  memory = recid(DayCampaign).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find DayCampaign where recid(DayCampaign) = rtab[frame-line] 
                    no-lock .
               RUN local-find-next.     
               if not available DayCampaign then do:
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
      
                  run local-disp-row.

                  rtab[frame-line] = recid(DayCampaign).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find DayCampaign where recid(DayCampaign) = memory no-lock no-error.
            RUN local-find-prev.
            if available DayCampaign then do:

               do i = 1 to (frame-down - 1):
                  RUN local-find-prev.
                  if available DayCampaign then memory = recid(DayCampaign).
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
        if lookup(nap,"1,f1") > 0 AND ufk[1] > 0 then 
        do on ENDkey undo, NEXT LOOP:

           /*lcEvent*/
           cfc = "puyr". RUN Syst/ufcolor.p.
           ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           set lcEvent with frame hayr.
           hide frame hayr no-pause.
           if lcEvent ENTERED then do:
             find first DayCampaign where 
                        DayCampaign.DCEvent >= lcEvent
             no-lock no-error.
             if not available DayCampaign then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
              end.
              /*  DayCampaign was found */
              assign
                memory = recid(DayCampaign)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find DayCampaign where recid(DayCampaign) = rtab[frame-line] no-lock.
           siirto = string(DayCampaign.DCEvent).
           leave MAIN.
        end. /* Choose */

        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           RUN local-find-first.
           memory = recid(DayCampaign).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           RUN local-find-last.
           memory = recid(DayCampaign).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.

PROCEDURE local-disp-row:

    display DayCampaign.DCEvent
            DayCampaign.DCName
            with frame sel.
            
END PROCEDURE. 

PROCEDURE local-find-first:

   FIND FIRST DayCampaign NO-LOCK NO-ERROR.
   IF AVAILABLE DayCampaign THEN DO:
      IF lcRestricted > "" THEN 
      DO WHILE LOOKUP(DayCampaign.DCEvent,lcRestricted) = 0:
         FIND NEXT DayCampaign NO-LOCK NO-ERROR.
         IF NOT AVAILABLE DayCampaign THEN LEAVE. 
      END.
      ELSE IF lcDCTypes > "" THEN 
      DO WHILE LOOKUP(STRING(DayCampaign.DCType),lcDCTypes) = 0:
         FIND NEXT DayCampaign NO-LOCK NO-ERROR.
         IF NOT AVAILABLE DayCampaign THEN LEAVE. 
      END.
   END.
   
END.

PROCEDURE local-find-last:

   FIND LAST DayCampaign NO-LOCK NO-ERROR.
   IF AVAILABLE DayCampaign THEN DO:
      IF lcRestricted > "" THEN 
      DO WHILE LOOKUP(DayCampaign.DCEvent,lcRestricted) = 0:
         FIND PREV DayCampaign NO-LOCK NO-ERROR.
         IF NOT AVAILABLE DayCampaign THEN LEAVE. 
      END.
      ELSE IF lcDCTypes > "" THEN 
      DO WHILE LOOKUP(STRING(DayCampaign.DCType),lcDCTypes) = 0:
         FIND PREV DayCampaign NO-LOCK NO-ERROR.
         IF NOT AVAILABLE DayCampaign THEN LEAVE. 
      END.
   END.
   
END.

PROCEDURE local-find-next:

   FIND NEXT DayCampaign NO-LOCK NO-ERROR.
   IF AVAILABLE DayCampaign THEN DO:
      IF lcRestricted > "" THEN 
      DO WHILE LOOKUP(DayCampaign.DCEvent,lcRestricted) = 0:
         FIND NEXT DayCampaign NO-LOCK NO-ERROR.
         IF NOT AVAILABLE DayCampaign THEN LEAVE. 
      END.
      ELSE IF lcDCTypes > "" THEN 
      DO WHILE LOOKUP(STRING(DayCampaign.DCType),lcDCTypes) = 0:
         FIND NEXT DayCampaign NO-LOCK NO-ERROR.
         IF NOT AVAILABLE DayCampaign THEN LEAVE. 
      END.
   END.
   
END.

PROCEDURE local-find-prev:

   FIND PREV DayCampaign NO-LOCK NO-ERROR.
   IF AVAILABLE DayCampaign THEN DO:
      IF lcRestricted > "" THEN 
      DO WHILE LOOKUP(DayCampaign.DCEvent,lcRestricted) = 0:
         FIND PREV DayCampaign NO-LOCK NO-ERROR.
         IF NOT AVAILABLE DayCampaign THEN LEAVE. 
      END.
      ELSE IF lcDCTypes > "" THEN 
      DO WHILE LOOKUP(STRING(DayCampaign.DCType),lcDCTypes) = 0:
         FIND PREV DayCampaign NO-LOCK NO-ERROR.
         IF NOT AVAILABLE DayCampaign THEN LEAVE. 
      END.
   END.
   
END.



