/*------------------------------------------------------
  MODULE .......: h-CLIType.p 
  PARENT .......: APPLHELP.P
  FUNCTION .....: HELP browser of billing type
  APPLICATION ..: NN
  AUTHOR .......: jp
  CREATED ......: 28-12-99
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/tmsconst.i}

DEF  shared VAR siirto AS CHAR.

DEF VAR ob-code     LIKE CLIType.Clitype     NO-UNDO. 
DEF VAR rtab        AS RECID EXTENT 11      NO-UNDO.
DEF VAR ufkey       AS LOG init TRUE        NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR Memory      AS RECID                NO-UNDO.
DEF VAR must-print  AS logic                NO-UNDO.
DEF VAR must-add    AS logic                NO-UNDO.

form
      CLIType.Clitype format "x(16)"
      CLIType.CliName  format "x(45)"
      CLIType.StatusCode
    WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(Syst.Var:cfc)
    title color value(Syst.Var:ctc) " CLI Type (" + Syst.Var:gcBrand + ") " OVERLAY FRAME sel.

form /* SEEK Code */
    ob-code
    help "Enter RepType of an Object Billing RepType"
    with row 4  col 2 title color value(Syst.Var:ctc) " FIND CODE "
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME hayr.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
MAIN:
repeat:

   FIND FIRST CLIType WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              LOOKUP(STRING(CLIType.WebStatusCode),
                     {&CLITYPE_WEB_ACTIVE_STATUSES}) > 0 AND
              LOOKUP(STRING(CLIType.StatusCode),
                     {&CLITYPE_STC_ACTIVE_STATUSES}) > 0 no-lock no-error.
   IF NOT AVAILABLE CLIType THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      Memory = recid(CLIType).
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
         FIND CLIType where recid(CLIType) = Memory no-lock no-error.

         /* Print TO screen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE CLIType:
            DISPLAY
            CLIType.Clitype
            CLIType.CliName
            CliType.StatusCode COLUMN-LABEL "Status"
            WITH FRAME sel.
            rtab[FRAME-LINE] = recid(CLIType).
            DOWN WITH FRAME sel.
            FIND NEXT CLIType WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              LOOKUP(STRING(CLIType.WebStatusCode),
                     {&CLITYPE_WEB_ACTIVE_STATUSES}) > 0 AND
              LOOKUP(STRING(CLIType.StatusCode),
                     {&CLITYPE_STC_ACTIVE_STATUSES}) > 0 no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(sel) - 1 WITH FRAME sel.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         Syst.Var:ufk = 0 Syst.Var:ufk[1] = 35 Syst.Var:ufk[5] = 11
         Syst.Var:ufk[6] = 0 Syst.Var:ufk[8] = 8  Syst.Var:ufk[9] = 1
         siirto = ? Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW CLIType.Clitype {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(Syst.Var:ccc) CLIType.Clitype WITH FRAME sel.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         Syst.Var:nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(Syst.Var:nap,"cursor-up") > 0 THEN DO
         WITH FRAME sel:
            IF FRAME-LINE = 1 THEN DO:
               FIND CLIType where recid(CLIType) = rtab[FRAME-LINE] no-lock.
               FIND PREV CLIType WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              LOOKUP(STRING(CLIType.WebStatusCode),
                     {&CLITYPE_WEB_ACTIVE_STATUSES}) > 0 AND
              LOOKUP(STRING(CLIType.StatusCode),
                     {&CLITYPE_STC_ACTIVE_STATUSES}) > 0 no-lock no-error.
               IF NOT AVAILABLE CLIType THEN DO:
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
                  DISPLAY CLIType.Clitype CLIType.CliName CLIType.StatusCode.
                  rtab[FRAME-LINE] = recid(CLIType).
                  Memory = recid(CLIType).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(Syst.Var:nap,"cursor-down") > 0 THEN DO WITH FRAME sel:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND CLIType where recid(CLIType) = rtab[FRAME-LINE] no-lock .
               FIND NEXT CLIType WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              LOOKUP(STRING(CLIType.WebStatusCode),
                     {&CLITYPE_WEB_ACTIVE_STATUSES}) > 0 AND
              LOOKUP(STRING(CLIType.StatusCode),
                     {&CLITYPE_STC_ACTIVE_STATUSES}) > 0 no-lock no-error.
               IF NOT AVAILABLE CLIType THEN DO:
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
                  DISPLAY CLIType.Clitype CLIType.CliName CLIType.StatusCode.
                  rtab[FRAME-LINE] = recid(CLIType).
                  /* finally LAST line's KeyValue is saved */
                  Memory = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(Syst.Var:nap,"page-up,prev-page") > 0 THEN DO WITH FRAME sel:
            FIND CLIType where recid(CLIType) = Memory no-lock no-error.
            FIND PREV CLIType WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              LOOKUP(STRING(CLIType.WebStatusCode),
                     {&CLITYPE_WEB_ACTIVE_STATUSES}) > 0 AND
              LOOKUP(STRING(CLIType.StatusCode),
                     {&CLITYPE_STC_ACTIVE_STATUSES}) > 0 no-lock no-error.
            IF AVAILABLE CLIType THEN DO:

               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND PREV CLIType WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              LOOKUP(STRING(CLIType.WebStatusCode),
                     {&CLITYPE_WEB_ACTIVE_STATUSES}) > 0 AND
              LOOKUP(STRING(CLIType.StatusCode),
                     {&CLITYPE_STC_ACTIVE_STATUSES}) > 0 no-lock no-error.
                  IF AVAILABLE CLIType THEN Memory = recid(CLIType).
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
        else if lookup(Syst.Var:nap,"page-down,next-page") > 0 THEN DO WITH FRAME sel:
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
        if lookup(Syst.Var:nap,"1,f1") > 0 THEN DO:  /* ob-code */
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           UPDATE ob-code WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           IF ob-code ENTERED THEN DO:
              FIND FIRST CLIType WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              LOOKUP(STRING(CLIType.WebStatusCode),
                     {&CLITYPE_WEB_ACTIVE_STATUSES}) > 0 AND
              LOOKUP(STRING(CLIType.StatusCode),
                     {&CLITYPE_STC_ACTIVE_STATUSES}) > 0 no-lock no-error.
               IF NOT AVAILABLE CLIType THEN DO:
                       BELL.
                       message "None found !".    
                       PAUSE 1 no-message.
                       NEXT BROWSE.
               END.
              /*  ob-code was found */
              ASSIGN
                Memory = recid(CLIType)
                must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Seek */

        /* CHOOSE */
        else if lookup(Syst.Var:nap,"return,enter,5,f5") > 0 THEN DO:
           FIND CLIType where recid(CLIType) = rtab[FRAME-LINE] no-lock.
           siirto = string(CLIType.Clitype).
           LEAVE MAIN.
        END. /* CHOOSE */
        /* FIRST record */
        else if lookup(Syst.Var:nap,"home,h") > 0 THEN DO:
           FIND FIRST CLIType WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              LOOKUP(STRING(CLIType.WebStatusCode),
                     {&CLITYPE_WEB_ACTIVE_STATUSES}) > 0 AND
              LOOKUP(STRING(CLIType.StatusCode),
                     {&CLITYPE_STC_ACTIVE_STATUSES}) > 0 no-lock no-error.
           Memory = recid(CLIType).
           must-print = TRUE.
           NEXT LOOP.
        END. /* FIRST record */

        /* LAST record */
        else if lookup(Syst.Var:nap,"end,e") > 0 THEN DO :
           FIND LAST CLIType WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              LOOKUP(STRING(CLIType.WebStatusCode),
                     {&CLITYPE_WEB_ACTIVE_STATUSES}) > 0 AND
              LOOKUP(STRING(CLIType.StatusCode),
                     {&CLITYPE_STC_ACTIVE_STATUSES}) > 0 no-lock no-error.
           Memory = recid(CLIType).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if Syst.Var:nap = "8" or Syst.Var:nap = "f8" THEN LEAVE MAIN. /* RETURN */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* MAIN */
HIDE FRAME sel no-pause.

