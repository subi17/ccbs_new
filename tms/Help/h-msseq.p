/*------------------------------------------------------
  MODULE .......: h-repcode.P
  PARENT .......: APPLHELP.P
  FUNCTION .....: HELP browser of report codes
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 30.12.2005
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF INPUT PARAM iiCustnum AS INTEGER NO-UNDO. 
DEF shared VAR siirto AS CHAR.

DEF VAR lcRepCode   AS CHAR                 NO-UNDO.
DEF VAR rtab        AS RECID EXTENT 11      NO-UNDO.
DEF VAR ufkey       AS LOG init TRUE        NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR memory      AS RECID                NO-UNDO.
DEF VAR must-print  AS logic                NO-UNDO.
DEF VAR must-add    AS logic                NO-UNDO.
DEF VAR lcSource    AS CHAR                 NO-UNDO.

DEF TEMP-TABLE ttMobsub NO-UNDO
    FIELD MsSeq  AS INT
    FIELD CLI AS CHAR
    INDEX MsSeq is unique MsSeq.

form
    ttMobsub.Msseq  label "Subscr.ID"
    ttMobsub.CLI Label "MSISDN"
    WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
    title color value(ctc) " Subscriptions " OVERLAY FRAME sel.

form /* SEEK code */
    lcRepCode
    help "Enter Name of a Report Code"
    with row 4 col 2 title color value(ctc) " FIND Subscription "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.

FOR EACH MobSub WHERE
         MobSub.Custnum = iiCustnum NO-LOCK:

    CREATE ttMobsub.
    ASSIGN ttMobsub.MsSeq = MobSub.MsSeq
           ttMobsub.CLI = MobSub.CLI.
END.

MAIN:
repeat:

   FIND FIRST ttMobsub no-lock no-error.
   IF NOT AVAILABLE ttMobsub THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      memory = recid(ttMobsub).
      must-print = TRUE.
   END.

   PAUSE 0.
   view FRAME sel.


   LOOP:
   Repeat WITH FRAME sel:

      print-line:
      DO :
         IF must-print THEN DO:
            CLEAR FRAME sel ALL no-pause.
            FIND ttMobsub where recid(ttMobsub) = memory no-lock no-error.

            /* Print TO screen */
            rtab = ?.
            DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE ttMobsub:
               DISPLAY
               ttMobsub.MsSeq
               ttMobsub.CLI
               WITH FRAME sel.
               
               rtab[FRAME-LINE] = recid(ttMobsub).
               DOWN WITH FRAME sel.
               FIND NEXT ttMobsub no-lock no-error.
            END.
            must-print = FALSE.
            up frame-line(sel) - 1 WITH FRAME sel.
         END. /* must-print */

         IF ufkey THEN DO:
            ASSIGN
            ufk = 0 /* ufk[1] = 35 ufk[5] = 11 */
            ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
            siirto = ? ehto = 3 ufkey = FALSE.
            
            /* not called from applhelp */    
            IF NOT gcHelpParam = "ahelp" THEN ufk[5] = 0.
            
            RUN Syst/ufkey.p.
         END.
     END. /* print-line */

     BROWSE:
     repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW ttMobsub.MsSeq ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) ttMobsub.MsSeq WITH FRAME sel.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME sel:
            IF FRAME-LINE = 1 THEN DO:
               FIND ttMobsub where recid(ttMobsub) = rtab[FRAME-LINE] no-lock.
               FIND prev ttMobsub no-lock no-error.
               IF NOT AVAILABLE ttMobsub THEN DO:
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
                  DISPLAY ttMobsub.MsSeq ttMobsub.CLI.
                  rtab[FRAME-LINE] = recid(ttMobsub).
                  memory = recid(ttMobsub).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME sel:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND ttMobsub where recid(ttMobsub) = rtab[FRAME-LINE] no-lock .
               FIND NEXT ttMobsub no-lock no-error.
               IF NOT AVAILABLE ttMobsub THEN DO:
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
                  DISPLAY ttMobsub.MsSeq ttMobsub.CLI.
                  rtab[FRAME-LINE] = recid(ttMobsub).
                  /* finally LAST line's keyvalue is saved */
                  memory = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME sel:
            FIND ttMobsub where recid(ttMobsub) = memory no-lock no-error.
            FIND prev ttMobsub no-lock no-error.
            IF AVAILABLE ttMobsub THEN DO:

               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev ttMobsub no-lock no-error.
                  IF AVAILABLE ttMobsub THEN memory = recid(ttMobsub).
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
               memory = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */
/*
        /* Seek */
        if lookup(nap,"1,f1") > 0 THEN DO:  /* RepCode */
           cfc = "puyr". RUN Syst/ufcolor.
           ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           lcRepCode = "".
           set lcRepCode WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           IF lcRepCode ENTERED THEN DO:
              FIND FIRST ttMobsub where ttMobsub.MsSeq >= lcRepCode
              no-lock no-error.
              IF NOT AVAILABLE ttMobsub THEN DO:
                 BELL.
                 message "None found !".    
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  RepCode was found */
              ASSIGN
                memory = recid(ttMobsub)
                must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Seek */
*/
        /* CHOOSE */
        else if lookup(nap,"return,enter,5,f5") > 0 /* AND ufk[5] > 0 */ THEN DO:
           FIND ttMobsub where recid(ttMobsub) = rtab[FRAME-LINE] no-lock.
           siirto = string(ttMobsub.MsSeq).
           LEAVE MAIN.
        END. /* CHOOSE */

        /* FIRST record */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST ttMobsub no-lock.
           memory = recid(ttMobsub).
           must-print = TRUE.
           NEXT LOOP.
        END. /* FIRST record */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST ttMobsub no-lock.
           memory = recid(ttMobsub).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE MAIN. /* RETURN */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* MAIN */

HIDE FRAME sel no-pause.

