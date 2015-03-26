/*------------------------------------------------------
  MODULE .......: CUSTSER.P
  PARENT .......: NNASSE.P
  FUNCTION .....: Search of Customer number Series
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 12.02.99
  MODIFIED .....: 09.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}

DEF TEMP-TABLE custser NO-UNDO
   FIELD ParamCode LIKE TMSParam.ParamCode
   FIELD CharVal LIKE TMSParam.CharVal

   INDEX ParamCode AS primary
      ParamCode.

DEF shared VAR siirto AS CHAR.

DEF VAR rtab        AS RECID EXTENT 11      NO-UNDO.
DEF VAR ufkey       AS LOG init TRUE        NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR upmost      AS RECID                NO-UNDO.
DEF VAR must-print  AS logic                NO-UNDO.
DEF VAR must-add    AS logic                NO-UNDO.
def var nro         as char format "x(5)"   NO-UNDO.
DEF VAR rows        AS INT                  NO-UNDO.

FOR EACH TMSParam no-lock where
         TMSParam.Brand      = gcBrand AND
         TMSParam.ParamGroup = "CustNumbers".

   CREATE custser.
   ASSIGN
      custser.ParamCode = TMSParam.ParamCode
      custser.CharVal = TMSParam.CharVal
      rows            = rows + 1.

END.

form
   custser.ParamCode column-label "Customer type"
   custser.CharVal column-label "Customer serie" format "x(15)"
WITH scroll 1 rows DOWN  ROW 4 centered COLOR value(cfc)
   title color value(ctc) " Number Series for New Customers " 
   OVERLAY FRAME tlse.

SCELE:
repeat:

   FIND FIRST custser no-lock no-error.
   IF AVAIL custser THEN ASSIGN
      upmost = recid(custser)
      must-print = TRUE.
   ELSE RETURN.

LOOP:
   Repeat WITH FRAME tlse:
print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND custser where recid(custser) = upmost no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE custser:
            DISPLAY
               custser.ParamCode
               custser.CharVal
            WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(custser).
            DOWN WITH FRAME tlse.
            FIND NEXT custser no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(tlse) - 1 WITH FRAME tlse.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW custser.ParamCode ;(uchoose.i;) no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) custser.ParamCode WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND custser where recid(custser) = rtab[FRAME-LINE] no-lock.
               FIND prev custser no-lock no-error.
               IF NOT AVAILABLE custser THEN DO:
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
                  DISPLAY 
                     custser.ParamCode
                     custser.CharVal.
                  rtab[FRAME-LINE] = recid(custser).
                  upmost = recid(custser).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND custser where recid(custser) = rtab[FRAME-LINE] no-lock .
               FIND NEXT custser no-lock no-error.
               IF NOT AVAILABLE custser THEN DO:
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
                  DISPLAY custser.ParamCode custser.CharVal.
                  rtab[FRAME-LINE] = recid(custser).
                  /* finally LAST line's KeyValue is saved */
                  upmost = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND custser where recid(custser) = upmost no-lock no-error.
            FIND prev custser no-lock no-error.
            IF AVAILABLE custser THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev custser no-lock no-error.
                  IF AVAILABLE custser THEN upmost = recid(custser).
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
        else if lookup(nap,"page-down,next-page") > 0 THEN DO WITH FRAME tlse:
           IF rtab[FRAME-DOWN] = ? THEN DO:
               BELL.
               message "This is the last page !".
               PAUSE 1 no-message.
           END.
           ELSE DO: /* the downmost line wasn't empty */
               upmost = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */

        /* Selection */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND custser where recid(custser) = rtab[FRAME-LINE] no-lock.
           siirto = custser.CharVal.
           LEAVE SCELE.
        END. /* Selection */

        /* FIRST record */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST custser no-lock.
           upmost = recid(custser).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST custser no-lock.
           upmost = recid(custser).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE SCELE. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* SCELE */
HIDE FRAME tlse no-pause.

