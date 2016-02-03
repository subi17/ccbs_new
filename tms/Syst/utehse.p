/* -----------------------------------------------
  MODULE .......: utehse.p
  FUNCTION .....: browse printer effects
  APPLICATION ..: nn
  CREATED ......: 13.04.99 pt    
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF INPUT PARAMETER  xPrinterId LIKE TMSPrinter.PrinterId NO-UNDO.
DEF OUTPUT PARAMETER xeff    AS c                  NO-UNDO.

xeff = ?.

def var ok as lo format "Yes/No" NO-UNDO.
DEF VAR memory   AS RECID              NO-UNDO.
def var line     as int format "99"    NO-UNDO.
DEF VAR delline    AS INT                NO-UNDO.
DEF VAR must-print AS LOG            NO-UNDO.
DEF VAR must-add    AS LOG            NO-UNDO.
DEF VAR ufkey        AS LOG            NO-UNDO.
DEF VAR fr-header AS CHAR.
DEF VAR rtab AS RECID EXTENT 24      NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR xrecid AS RECID.
DEF VAR idx AS INT.
DEF VAR teho AS CHAR NO-UNDO.

form
    PrintCodes.Effect   column-label "Code"
    help "Code for effect"
    EffName          column-label "NameOfEffect"
    help "Name/Purpose"
    PrintCodes.PageWidth  column-label "Max.width"
    PrintCodes.PageLength  column-label "Lines/page"
    PrintCodes.AvailLines column-label "Use lines/page"
    WITH ROW 2 col 2 centered  OVERLAY scroll 1 13 DOWN
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    " PRINTER '" + xPrinterId + "' EFFECTS "
    FRAME sel.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.
FIND FIRST TMSPrinter where TMSPrinter.PrinterId = xPrinterId no-lock no-error.
FIND FIRST PrintCodes where PrintCodes.PrinterId = xPrinterId no-lock no-error.
IF AVAILABLE PrintCodes THEN DO:
   ASSIGN
   memory = recid(PrintCodes)
   must-print = TRUE
   must-add    = FALSE.
END.
ELSE DO:
   BELL.
   message "This printer has no effects defined - press ENTER !".
   PAUSE no-message.
   RETURN.
END.
ASSIGN xrecid = ? delline = 0 ufkey = TRUE.

LOOP:
repeat WITH FRAME sel ON ENDKEY UNDO LOOP, NEXT LOOP:

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND PrintCodes where recid(PrintCodes) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         alkaen lineltA delline */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE PrintCodes THEN DO:
               DISPLAY PrintCodes.Effect EffName
                       PrintCodes.PageWidth PrintCodes.PageLength PrintCodes.AvailLines.
               rtab[FRAME-LINE] = recid(PrintCodes).
               FIND NEXT PrintCodes where PrintCodes.PrinterId = xPrinterId
               no-lock no-error.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.

         up FRAME-LINE - 1.
         must-print = FALSE.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         mAllA linellA choosea odotellen. */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   delline=0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 0   ufk[2]= 0    ufk[3]= 0 ufk[4]= 0
         ufk[5]= 11  ufk[6]= 0    ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      CHOOSE ROW PrintCodes.EffName ;(uchoose.i;) no-error WITH FRAME sel.
      COLOR DISPLAY normal PrintCodes.EffName WITH FRAME sel.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "You are on an empty line, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND PrintCodes where recid(PrintCodes) = rtab[1] no-lock.
            FIND prev PrintCodes where PrintCodes.PrinterId = xPrinterId no-lock no-error.
            IF NOT AVAILABLE PrintCodes THEN DO:
               message "YOU ARE ON THE FIRTS ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY PrintCodes.Effect EffName
                       PrintCodes.PageWidth PrintCodes.PageLength PrintCodes.AvailLines.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(PrintCodes)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:

         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND PrintCodes where recid(PrintCodes) = rtab[FRAME-DOWN] no-lock .
            FIND NEXT PrintCodes where PrintCodes.PrinterId = xPrinterId no-lock no-error.
            IF NOT AVAILABLE PrintCodes THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* was found vielA seuraava tietue */
               scroll up.
               DISPLAY PrintCodes.Effect EffName
                       PrintCodes.PageWidth PrintCodes.PageLength PrintCodes.AvailLines.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(PrintCodes).
               /* ja lopuksi pannaan memoryin ylimmAn linen avain */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
         memory = rtab[1].
         FIND PrintCodes where recid(PrintCodes) = memory no-lock no-error.
         FIND prev PrintCodes where PrintCodes.PrinterId = xPrinterId no-lock no-error.

         IF AVAILABLE PrintCodes THEN DO:
            memory = recid(PrintCodes).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               FIND prev PrintCodes where PrintCodes.PrinterId = xPrinterId
               no-lock no-error.
               IF AVAILABLE PrintCodes THEN memory = recid(PrintCodes).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:

        /* cursor TO the downmost line */

        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND PrintCodes where recid(PrintCodes) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     else if lookup(nap,"enter,return,5,f5") > 0 THEN DO: /* CHOOSE */

        FIND PrintCodes where recid(PrintCodes) = rtab[frame-line(sel)]
        no-lock.

        ASSIGN
        xeff = PrintCodes.Effect.
        LEAVE LOOP.
     END.   

     else if lookup(nap,"e,end") > 0 THEN DO : /* LAST record */
        FIND LAST PrintCodes where PrintCodes.PrinterId = xPrinterId no-lock.
        ASSIGN
        memory = recid(PrintCodes)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"h,home") > 0 THEN DO:
        FIND FIRST PrintCodes where PrintCodes.PrinterId = xPrinterId no-lock.
        ASSIGN
        memory = recid(PrintCodes)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.


