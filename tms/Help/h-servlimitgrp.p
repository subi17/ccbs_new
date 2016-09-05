/*------------------------------------------------------
  MODULE .......: h-ServiceLimitGroup.p 
  PARENT .......: APPLHELP.P
  FUNCTION .....: HELP browser of billing type
  APPLICATION ..: NN
  AUTHOR .......: jp
  CREATED ......: 28-12-99
  MODIFIED .....: 
  Version ......: 5.1
  ------------------------------------------------------ */

{Syst/commali.i}

DEF  shared VAR siirto AS CHAR.

DEF VAR ob-code     LIKE ServiceLimitGroup.Groupcode  NO-UNDO. 
DEF VAR rtab        AS RECID EXTENT 11      NO-UNDO.
DEF VAR ufkey       AS LOG init TRUE        NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR Memory      AS RECID                NO-UNDO.
DEF VAR must-print  AS logic                NO-UNDO.
DEF VAR must-add    AS logic                NO-UNDO.

form
      ServiceLimitGroup.GroupCode
      ServiceLimitGroup.GroupName  format "x(30)"
    WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
    title color value(ctc) " ServiceLimit Groups " OVERLAY FRAME sel.

form /* SEEK Code */
    ob-code
    help "Enter RepType of an Object Billing RepType"
    with row 4  col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
MAIN:
repeat:

   FIND FIRST ServiceLimitGroup no-lock no-error.
   IF NOT AVAILABLE ServiceLimitGroup THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      Memory = recid(ServiceLimitGroup).
      must-print = TRUE.
   END.

PAUSE 0.
view FRAME  sel.

LOOP:
   Repeat WITH FRAME sel:

print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME sel ALL no-pause.
         FIND ServiceLimitGroup where recid(ServiceLimitGroup) = Memory no-lock no-error.

         /* Print TO screen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE ServiceLimitGroup:
            DISPLAY
            ServiceLimitGroup.GroupCode
            ServiceLimitGroup.GroupName
            WITH FRAME sel.
            rtab[FRAME-LINE] = recid(ServiceLimitGroup).
            DOWN WITH FRAME sel.
            FIND NEXT ServiceLimitGroup no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(sel) - 1 WITH FRAME sel.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW ServiceLimitGroup.GroupCode {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) ServiceLimitGroup.GroupCode WITH FRAME sel.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME sel:
            IF FRAME-LINE = 1 THEN DO:
               FIND ServiceLimitGroup where recid(ServiceLimitGroup) = rtab[FRAME-LINE] no-lock.
               FIND prev ServiceLimitGroup no-lock no-error.
               IF NOT AVAILABLE ServiceLimitGroup THEN DO:
                  BELL.
                  message "You are on 1st row !".              
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* a previous one was found */
                  scroll DOWN.
                  DO i = 11 TO 2 BY -1:
                     rtab[i] = rtab[i - 1].
                  END.
                  DISPLAY ServiceLimitGroup.GroupCode ServiceLimitGroup.GroupName.
                  rtab[FRAME-LINE] = recid(ServiceLimitGroup).
                  Memory = recid(ServiceLimitGroup).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME sel:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND ServiceLimitGroup where recid(ServiceLimitGroup) = rtab[FRAME-LINE] no-lock .
               FIND NEXT ServiceLimitGroup no-lock no-error.
               IF NOT AVAILABLE ServiceLimitGroup THEN DO:
                  BELL.
                  message "You are on last row !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* yet another record was found */
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.
                  DISPLAY ServiceLimitGroup.GroupCode ServiceLimitGroup.GroupName.
                  rtab[FRAME-LINE] = recid(ServiceLimitGroup).
                  /* finally LAST line's KeyValue is saved */
                  Memory = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME sel:
            FIND ServiceLimitGroup where recid(ServiceLimitGroup) = Memory no-lock no-error.
            FIND prev ServiceLimitGroup no-lock no-error.
            IF AVAILABLE ServiceLimitGroup THEN DO:

               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev ServiceLimitGroup no-lock no-error.
                  IF AVAILABLE ServiceLimitGroup THEN Memory = recid(ServiceLimitGroup).
                  ELSE i = FRAME-DOWN.
               END.
               must-print = TRUE.
               NEXT LOOP.
            END.
            ELSE DO:
               /* this is the FIRST data page */
               BELL.
               message "This is the 1st page !".          
               PAUSE 1 no-message.
            END.
        END. /* previous page */

        /* NEXT page */
        else if lookup(nap,"page-down,next-page") > 0 THEN DO WITH FRAME sel:
           IF rtab[FRAME-DOWN] = ? THEN DO:
               BELL.
               message "This is the last page !".
               PAUSE 1 no-message.
           END.
           ELSE DO: /* the downmost line wasn't empty */
               Memory = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */

        /* Seek */
        if lookup(nap,"1,f1") > 0 THEN DO:  /* ob-code */
           cfc = "puyr". RUN Syst/ufcolor.p.
           ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           UPDATE ob-code WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           IF ob-code ENTERED THEN DO:
              FIND FIRST ServiceLimitGroup where ServiceLimitGroup.GroupCode >= ob-code
              no-lock no-error.
               IF NOT AVAILABLE ServiceLimitGroup THEN DO:
                       BELL.
                       message "None found !".    
                       PAUSE 1 no-message.
                       NEXT BROWSE.
               END.
              /*  ob-code was found */
              ASSIGN
                Memory = recid(ServiceLimitGroup)
                must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Seek */

        /* CHOOSE */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND ServiceLimitGroup where recid(ServiceLimitGroup) = rtab[FRAME-LINE] no-lock.
           siirto = string(ServiceLimitGroup.GroupCode).
           LEAVE MAIN.
        END. /* CHOOSE */
        /* FIRST record */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST ServiceLimitGroup no-lock.
           Memory = recid(ServiceLimitGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* FIRST record */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST ServiceLimitGroup no-lock.
           Memory = recid(ServiceLimitGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE MAIN. /* RETURN */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* MAIN */
HIDE FRAME sel no-pause.

