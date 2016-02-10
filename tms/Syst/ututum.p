/* -----------------------------------------------------------------------
  MODULE .......: ututum.p
  FUNCTION .....: TulostumAAritysten yllApito
  APPLICATION ..: VP
  AUTHOR .......: TT
  CREATED ......: 27-06-91
  MODIFIED .....: 25.09.96 /tt --> Ruotsinnettu
                  25.06.98 /kl --> vast => ok
                  13.04.99 /pt --> F9 activated on Effect code
                  17.09.02 /jp --> more validation
                  26.09.02 /jr --> fixed loop
                  16.10.02/aam     si-tul is shared not new shared,
                                   UserCode can be empty
                  06.11.02/jr Eventlog
                  06.03.03/tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TMSRepCfg'}
DEF NEW shared VAR si-kirj AS CHAR.
DEF shared VAR si-tul LIKE TMSPrinter.PrinterId NO-UNDO init "memo".

def var ok          as lo format "Yes/No" NO-UNDO.
DEF VAR memory      AS RECID              NO-UNDO.
def var line        as int format "99"    NO-UNDO.
DEF VAR delline     AS INT                NO-UNDO.
DEF VAR must-print  AS LOG            NO-UNDO.
DEF VAR must-add    AS LOG            NO-UNDO.
DEF VAR ufkey       AS LOG            NO-UNDO.
DEF VAR fr-header   AS CHAR.
DEF VAR rtab        AS RECID EXTENT 24      NO-UNDO.
DEF VAR i           AS INT NO-UNDO.
DEF VAR xrecid      AS RECID.
DEF VAR tnimi       LIKE PrintCodes.EffName.
DEF VAR xeff        AS c                  NO-UNDO.

DEF BUFFER xxrepcfg FOR TMSRepCfg.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTMSRepCfg AS HANDLE NO-UNDO.
   lhTMSRepCfg = BUFFER TMSRepCfg:HANDLE.
   RUN StarEventInitialize(lhTMSRepCfg).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhTMSRepCfg).
   END.
END.

form
    TMSRepCfg.UserCode   column-label "User ID"
    help "User ID (empty: definition valid for ALL users exept named ones)"
    TMSRepCfg.PrinterId  column-label "Printer"
    tnimi           column-label "Effect"
 WITH ROW 2 centered OVERLAY scroll 1 13 DOWN
    COLOR value(cfc) TITLE COLOR value(ctc)
    " " + TMSReport.Memo + ": PRINT PARAMETERS "
    FRAME sel.

form
    TMSRepCfg.PrinterId           label "Printer ....."
    help "Printer's logical name" SKIP
    TMSRepCfg.Effect format "#x" label "Effect ......"
    help "Printer effect" 
    tnimi NO-LABEL SKIP
    TMSRepCfg.UserCode            label "User ........"
    help "User ID or empty for ALL users" SKIP
 WITH  OVERLAY ROW 8 centered COLOR value(cfc)
    TITLE COLOR value(ctc) fr-header WITH side-labels
    FRAME lis.

FIND FIRST TMSReport where TMSReport.RepName = si-tul no-lock no-error.
cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.
FIND FIRST TMSRepCfg where TMSRepCfg.RepName = si-tul no-lock no-error.
IF AVAILABLE TMSRepCfg THEN DO:
   ASSIGN
   si-kirj = TMSRepCfg.PrinterId
   memory = recid(TMSRepCfg)
   must-print = TRUE
   must-add    = FALSE.
END.
ELSE DO:
   ASSIGN
   memory = ?
   must-print = FALSE
   must-add    = TRUE.
END.
ASSIGN xrecid = ? delline = 0 ufkey = TRUE.

LOOP:
repeat WITH FRAME sel:

   IF must-add THEN DO:  /* TMSRepCfg -ADD  */
      ASSIGN
      cfc = "lis"
      ufkey = TRUE
      fr-header = " ADD, (width " + string(TMSReport.PageWidth) + ") ".
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis:
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN Syst/ufkey.
         assign tnimi = "".

         CREATE TMSRepCfg.
         ASSIGN TMSRepCfg.RepName = si-tul.
         UPDATE
                TMSRepCfg.PrinterId
                validate(TMSRepCfg.PrinterId = "" OR can-find(TMSPrinter where
                         TMSPrinter.PrinterId = INPUT TMSRepCfg.PrinterId),
                         "Printer " + input TMSRepCfg.PrinterId + 
                         " is not found !")
                TMSRepCfg.Effect
                TMSRepCfg.UserCode
                validate(TMSRepCfg.UserCode = "" OR  can-find(TMSUser where
                         TMSUser.UserCode = INPUT TMSRepCfg.UserCode),
                         "User " + input TMSRepCfg.UserCode + 
                         " does not exist !")
         EDITING:
            READKEY.
            IF keylabel(lastkey) = "F4" THEN 
            DO:
              must-add = false.
              undo,leave add-new.
            end.  
            if frame-field = "Effect" and keylabel(lastkey) = "F9"
            THEN DO:
               RUN Syst/utehse(INPUT INPUT TMSRepCfg.PrinterId,
                          OUTPUT xeff).
               IF xeff NE ? THEN DISP xeff @ TMSRepCfg.Effect.
               NEXT.
            END.

            if frame-field = "UserCode" AND
            lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:

               if CAN-FIND(xxRepcfg WHERE
                           xxRepCfg.Repname   = TMSRepCfg.Repname   AND
                           xxRepCfg.Effect    = TMSRepCfg.Effect    AND
                           xxRepCfg.PrinterID = si-kirj  AND
                           xxRepCfg.userCode  = Input TMSRepCfg.usercode ) 
               THEN DO:             
                  BELL.
                  MESSAGE
                  "Printer:" STRING(si-kirj,"x(15)")            SKIP
                  "Effect :" STRING(TMSRepCfg.Effect,"x(15)")   SKIP
                  "User   :" STRING(INPUT TMSRepCfg.usercode,"x(15)") SKIP
                  VIEW-AS ALERT-BOX TITLE "Already exists". 
                  NEXT-PROMPT TMSRepCfg.usercode. NEXT.
               END.   
            END.


            if frame-field = "PrinterId" AND
            lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
               if input TMSRepCfg.PrinterId = "" THEN UNDO, LEAVE add-new.
               ASSIGN si-kirj = INPUT TMSRepCfg.PrinterId.
               APPLY LASTKEY.
            END.

            else if frame-field = "Effect" AND
            lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
               ASSIGN TMSRepCfg.Effect = INPUT TMSRepCfg.Effect.
               TMSRepCfg.Effect = caps(TMSRepCfg.Effect).
               if input TMSRepCfg.Effect = "" then TMSRepCfg.Effect = "E".
               FIND PrintCodes where PrintCodes.PrinterId = 
               INPUT TMSRepCfg.PrinterId AND
               PrintCodes.Effect = TMSRepCfg.Effect no-lock no-error.
               IF AVAILABLE PrintCodes THEN DO:
                  tnimi = PrintCodes.EffName.
               END.
               ELSE DO:
                  tnimi = "".
                  BELL.
                  message "Effect " TMSRepCfg.Effect " is not found !".
                  NEXT.
               END.
               IF PrintCodes.PageWidth < TMSReport.PageWidth THEN DO:
                  DISPLAY tnimi WITH FRAME lis.
                  BELL.
                  MESSAGE 
                  "Effect '" + tnimi +
                          "' is too narrow (no. of chars per line) for ".
                  message "printout " + TMSReport.Memo + "'!".
                  NEXT.
               END.
               DISPLAY TMSRepCfg.Effect tnimi WITH FRAME lis.
               APPLY LASTKEY.
            END.
            ELSE APPLY LASTKEY.
         END.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTMSRepCfg).

         ASSIGN
         memory = recid(TMSRepCfg)
         xrecid = memory.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST TMSRepCfg where TMSRepCfg.RepName = si-tul no-lock no-error.
      IF NOT AVAILABLE TMSRepCfg THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND TMSRepCfg where recid(TMSRepCfg) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         alkaen lineltA delline */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE TMSRepCfg THEN DO:
               FIND FIRST PrintCodes where PrintCodes.PrinterId = 
               TMSRepCfg.PrinterId AND
               PrintCodes.Effect = TMSRepCfg.Effect no-lock no-error.
               IF AVAILABLE PrintCodes THEN ASSIGN tnimi = PrintCodes.EffName.
               else assign tnimi  = "** UNKNOWN **".
               DISPLAY TMSRepCfg.UserCode TMSRepCfg.PrinterId tnimi.
               rtab[FRAME-LINE] = recid(TMSRepCfg).
               FIND NEXT TMSRepCfg where TMSRepCfg.RepName = si-tul 
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
         ASSIGN must-print = FALSE.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         mAllA linellA choosea odotellen. */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 0  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      CHOOSE ROW TMSRepCfg.UserCode ;(uchoose.i;) no-error WITH FRAME sel.
      COLOR DISPLAY value(ccc) TMSRepCfg.UserCode WITH FRAME sel.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "You are on an empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND TMSRepCfg where recid(TMSRepCfg) = rtab[1] no-lock.
            FIND prev TMSRepCfg where TMSRepCfg.RepName = si-tul 
            no-lock no-error.
            IF NOT AVAILABLE TMSRepCfg THEN DO:
               message "THIS IS THE FIRST ROW !". 
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               FIND FIRST PrintCodes where PrintCodes.PrinterId = 
               TMSRepCfg.PrinterId AND
               PrintCodes.Effect = TMSRepCfg.Effect no-lock no-error.
               IF AVAILABLE PrintCodes THEN ASSIGN tnimi = PrintCodes.EffName.
               else assign tnimi  = "** UNKNOWN **".
               DISPLAY TMSRepCfg.UserCode TMSRepCfg.PrinterId tnimi.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(TMSRepCfg)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:

         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND TMSRepCfg where recid(TMSRepCfg) = rtab[FRAME-DOWN] no-lock .
            FIND NEXT TMSRepCfg where TMSRepCfg.RepName = si-tul
            no-lock no-error.
            IF NOT AVAILABLE TMSRepCfg THEN DO:
               message "THIS IS THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* was found vielA seuraava tietue */
               scroll up.
               FIND FIRST PrintCodes where PrintCodes.PrinterId = 
               TMSRepCfg.PrinterId AND
               PrintCodes.Effect = TMSRepCfg.Effect no-lock no-error.
               IF AVAILABLE PrintCodes THEN ASSIGN tnimi = PrintCodes.EffName.
               else assign tnimi  = "** UNKNOWN **".
               DISPLAY TMSRepCfg.UserCode TMSRepCfg.PrinterId tnimi.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(TMSRepCfg).
               /* ja lopuksi pannaan memoryin ylimmAn linen avain */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
         memory = rtab[1].
         FIND TMSRepCfg where recid(TMSRepCfg) = memory no-lock no-error.
         FIND prev TMSRepCfg where TMSRepCfg.RepName = si-tul no-lock no-error.

         IF AVAILABLE TMSRepCfg THEN DO:
            memory = recid(TMSRepCfg).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               FIND prev TMSRepCfg where TMSRepCfg.RepName = si-tul 
               no-lock no-error.
               IF AVAILABLE TMSRepCfg THEN memory = recid(TMSRepCfg).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "THIS IS THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:

        /* cursor TO the downmost line */

        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "THIS IS THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND TMSRepCfg where recid(TMSRepCfg) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisAys */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" THEN DO:  /* removal */
        delline = FRAME-LINE.
        FIND TMSRepCfg where recid(TMSRepCfg) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) TMSRepCfg.UserCode TMSRepCfg.PrinterId
                                 tnimi WITH FRAME sel.

        FIND NEXT TMSRepCfg where TMSRepCfg.RepName = si-tul no-lock no-error.
        IF AVAILABLE TMSRepCfg THEN memory = recid(TMSRepCfg).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND TMSRepCfg where recid(TMSRepCfg) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           FIND prev TMSRepCfg where TMSRepCfg.RepName = si-tul 
           no-lock no-error.
           IF AVAILABLE TMSRepCfg THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(TMSRepCfg).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND TMSRepCfg where recid(TMSRepCfg) = rtab[FRAME-LINE] exclusive-lock.

        ASSIGN ok = FALSE.
        message "ARE YOU SURE YOU WANT TO REMOVE (Y/N)? " UPDATE ok.
        COLOR DISPLAY value(ccc) TMSRepCfg.UserCode TMSRepCfg.PrinterId
                                 tnimi WITH FRAME sel.
        IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMSRepCfg).
            DELETE TMSRepCfg.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST TMSRepCfg where TMSRepCfg.RepName = si-tul)
            THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 AND lcRight = "RW"
     THEN DO WITH FRAME lis:
        /* change */
        FIND TMSRepCfg where recid(TMSRepCfg) = rtab[frame-line(sel)]
        exclusive-lock.

        assign fr-header = " CHANGE (Width: " 
                           + string(TMSReport.PageWidth) 
                                    + ")".

        ASSIGN ufkey = TRUE ehto = 9.
        RUN Syst/ufkey.
        cfc = "lis". RUN Syst/ufcolor.

        FIND FIRST PrintCodes where 
                   PrintCodes.Effect = TMSRepCfg.Effect AND 
                   PrintCodes.PrinterId = TMSRepCfg.PrinterId
        no-lock no-error.

        IF AVAILABLE PrintCodes 
        THEN ASSIGN tnimi = PrintCodes.EffName.
        else assign tnimi  = "** Existerar ej **".

        DISPLAY tnimi WITH FRAME lis.

        ASSIGN si-kirj = TMSRepCfg.PrinterId.

        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMSRepCfg).

        UPDATE
               TMSRepCfg.PrinterId
               validate(TMSRepCfg.PrinterId = "" OR can-find(TMSPrinter where
                        TMSPrinter.PrinterId = INPUT TMSRepCfg.PrinterId),
                        "Printern " + INPUT TMSRepCfg.PrinterId + 
                        " does not exist !")
               TMSRepCfg.Effect
               TMSRepCfg.UserCode
               validate(TMSRepCfg.UserCode = "" OR can-find(TMSUser where
                        TMSUser.UserCode = INPUT TMSRepCfg.UserCode),
                        "User " + input TMSRepCfg.UserCode + 
                        " does not exist !")
        EDITING:
            READKEY.

            if frame-field = "Effect" and keylabel(lastkey) = "F9"
            THEN DO:
               RUN Syst/utehse(INPUT INPUT TMSRepCfg.PrinterId,
                          OUTPUT xeff).
               IF xeff NE ? THEN DISP xeff @ TMSRepCfg.Effect.
               NEXT.
            END.

            if frame-field = "PrinterId" AND
            lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
               ASSIGN si-kirj = INPUT TMSRepCfg.PrinterId.
               APPLY LASTKEY.
            END.
            else if frame-field = "Effect" AND
            lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
               ASSIGN TMSRepCfg.Effect = INPUT TMSRepCfg.Effect.
               TMSRepCfg.Effect = caps(TMSRepCfg.Effect).
               if input TMSRepCfg.Effect = "" then TMSRepCfg.Effect = "E".
               FIND PrintCodes where PrintCodes.PrinterId = 
               INPUT TMSRepCfg.PrinterId AND
               PrintCodes.Effect = TMSRepCfg.Effect no-lock no-error.
               IF AVAILABLE PrintCodes THEN DO:
                  tnimi = PrintCodes.EffName.
               END.
               ELSE DO:
                  tnimi = "".
                  BELL.
                  message "Effect " TMSRepCfg.Effect " is not found !".
                  NEXT.
               END.
               IF PrintCodes.PageWidth < TMSReport.PageWidth THEN DO:
                  DISPLAY tnimi WITH FRAME lis.
                  BELL.
                  message "Effect '" + tnimi +
                          "' does not suit for printout '" +
                          TMSReport.Memo + "'!".
                  NEXT.
               END.
               DISPLAY TMSRepCfg.Effect tnimi WITH FRAME lis.
               APPLY LASTKEY.
            END.
            ELSE APPLY LASTKEY.
        END.
        HIDE FRAME lis no-pause.

        FIND FIRST PrintCodes where 
                   PrintCodes.PrinterId = TMSRepCfg.PrinterId AND
                   PrintCodes.Effect = TMSRepCfg.Effect 
        no-lock no-error.

        IF AVAILABLE PrintCodes 
        THEN ASSIGN tnimi = PrintCodes.EffName.
        else assign tnimi  = "** UNKNOWN **".

        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMSRepCfg).

        DISPLAY TMSRepCfg.UserCode TMSRepCfg.PrinterId tnimi WITH FRAME sel.
        xrecid = recid(TMSRepCfg).
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* LAST record */
        FIND LAST TMSRepCfg where TMSRepCfg.RepName = si-tul no-lock.
        ASSIGN
        memory = recid(TMSRepCfg)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"home") > 0 THEN DO:
        FIND FIRST TMSRepCfg where TMSRepCfg.RepName = si-tul no-lock.
        ASSIGN
        memory = recid(TMSRepCfg)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

