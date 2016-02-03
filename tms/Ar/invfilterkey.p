/*------------------------------------------------------
  Module .......: invfilterkey.p
  FUNCTION .....: Choose key for invoice filtering 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 22.11.07
  MODIFIED .....: 
  Version ......: Yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Ar/invfilterkey.i}

DEF INPUT  PARAMETER TABLE FOR ttFilter.
DEF INPUT  PARAMETER iiFilterType AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER ocFilterKey  AS CHAR NO-UNDO.
                    
DEF VAR rtab        AS RECID NO-UNDO EXTENT 11.
DEF VAR ufkey       AS LOG   NO-UNDO.
DEF VAR i           AS INT   NO-UNDO.
DEF VAR memory      AS RECID NO-UNDO.
DEF VAR must-print  AS LOG   NO-UNDO.
DEF VAR must-add    AS LOG   NO-UNDO.
DEF VAR lcKey       AS CHAR  NO-UNDO.
DEF VAR lcFilter    AS CHAR  NO-UNDO.
DEF VAR liFilter    AS INT   NO-UNDO.

FORM
   lcKey         FORMAT "X(30)"   LABEL "Filtering Key" 
   ttFilter.FQty FORMAT ">>>>>>9" LABEL "Invoice Qty (All)"
   WITH SCROLL 1 11 DOWN ROW 4 CENTERED COLOR VALUE(cfc)
        TITLE COLOR VALUE(ctc) " CHOOSE KEY " OVERLAY FRAME sel.

cfc = "sel". run ufcolor. assign ccc = cfc.

ASSIGN
   ufkey      = TRUE
   must-print = TRUE.

RUN local-find-FIRST.
IF NOT AVAILABLE ttFilter THEN RETURN. 
memory = RECID(ttFilter).

MAIN:
REPEAT:

pause 0.
view frame  sel.

LOOP:
REPEAT with frame sel:

   print-line:
   do:
      if must-print then do:
         clear frame sel all no-pause.
         
         find ttFilter where recid(ttFilter) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available ttFilter:
         
            run local-disp-row.

            rtab[frame-line] = recid(ttFilter).
            down with frame sel.
         
            RUN local-find-NEXT.
         end.
      
         must-print = false.
         up frame-line(sel) - 1 with frame sel.

      end. /* must-print */

      if ufkey then do:
         assign
            ufk    = 0 
            ufk[5] = 11
            ufk[8] = 8 
            ehto   = 3 
            ufkey  = false.

         run ufkey.
      end.

   end. /* print-line */

   BROWSE:
   REPEAT with frame sel on endkey undo, retuRN:

      hide message no-pause.
      choose row lcKey ;(uchoose.i;) no-error with frame sel.
      color display value(ccc) lcKey with frame sel.

      nap = keylabel(lastkey).

      if frame-value = "" and rtab[frame-line] = ? and
         lookup(nap,"8,f8") = 0
      then next.

      /* previous line */
      if lookup(nap,"cursor-up") > 0 then do
      with frame sel:
   
         if frame-line = 1 then do:
          
            find ttFilter where recid(ttFilter) = rtab[frame-line] no-lock.
            RUN local-find-PREV.

            if not available ttFilter then do:
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

               rtab[frame-line] = recid(ttFilter).
               memory = recid(ttFilter).
            end.
         end.
         else up 1.
      end. /* previous line */

      /* next line */
      if lookup(nap,"cursor-down") > 0 then do with frame sel:
         if frame-line = frame-down then do:
         
            find ttFilter where recid(ttFilter) = rtab[frame-line] no-lock .

            RUN local-find-NEXT.
            
            if not available ttFilter then do:
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

               rtab[frame-line] = recid(ttFilter).
               /* finally last line's KeyValue is saved */
               memory = rtab[1].
            end.
         end.
         else down 1 .
      end. /* next line */

      /* previous page */
      else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
         find ttFilter where recid(ttFilter) = memory no-lock no-error.
         
         RUN local-find-PREV.
            
         if available ttFilter then do:

            do i = 1 to (frame-down - 1):
 
          
               RUN local-find-PREV.

               if available ttFilter then memory = recid(ttFilter).
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

      /* Choose */
      else if lookup(nap,"return,enter,5,f5") > 0 then do:
         find ttFilter where recid(ttFilter) = rtab[frame-line] no-lock.
         ocFilterKey = ttFilter.FCharKey.
         leave MAIN.
      end. /* Choose */

      /* First record */
      else if lookup(nap,"home,h") > 0 then do:

         RUN local-find-FIRST.

         memory = recid(ttFilter).
         must-print = true.
         next LOOP.
      end. /* First record */

      /* last record */
      else if lookup(nap,"end,e") > 0 then do :

         RUN local-find-LAST.
        
         memory = recid(ttFilter).
         must-print = true.
         next LOOP.
      end. /* last record */

      else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

   end.  /* BROWSE */
end.  /* LOOP */

end. /* MAIN */
hide frame sel no-pause.

PROCEDURE local-disp-row:

    lcKey = ttFilter.FCharKey.

    IF iiFilterType = 3 THEN 
       lcKey = lcKey + " " + 
               DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "Invoice",
                                "InvType",
                                ttFilter.FCharKey).

    DISPLAY 
       lcKey 
       ttFilter.FQty
    WITH FRAME sel.
            
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiFilterType = 1 
   THEN FIND FIRST ttFilter USE-INDEX FChar WHERE
                   ttFilter.FType = iiFilterType NO-ERROR.
   ELSE FIND FIRST ttFilter USE-INDEX FInt WHERE
                   ttFilter.FType = iiFilterType  NO-ERROR.

END PROCEDURE.   

PROCEDURE local-find-LAST:

   IF iiFilterType = 1 
   THEN FIND LAST ttFilter USE-INDEX FChar WHERE
                  ttFilter.FType = iiFilterType NO-ERROR.
   ELSE FIND LAST ttFilter USE-INDEX FInt WHERE
                  ttFilter.FType = iiFilterType  NO-ERROR.

END PROCEDURE.   


PROCEDURE local-find-NEXT:

   IF iiFilterType = 1 
   THEN FIND NEXT ttFilter USE-INDEX FChar WHERE
                  ttFilter.FType = iiFilterType NO-ERROR.
   ELSE FIND NEXT ttFilter USE-INDEX FInt WHERE
                  ttFilter.FType = iiFilterType  NO-ERROR.

END PROCEDURE.   

PROCEDURE local-find-PREV:

   IF iiFilterType = 1 
   THEN FIND PREV ttFilter USE-INDEX FChar WHERE
                  ttFilter.FType = iiFilterType NO-ERROR.
   ELSE FIND PREV ttFilter USE-INDEX FInt WHERE
                  ttFilter.FType = iiFilterType  NO-ERROR.

END PROCEDURE.   


