/* -----------------------------------------------
  MODULE .......: utukir.p
  FUNCTION .....: Kirjoitintietojen yllApito
  APPLICATION ..: VP
  AUTHOR .......: TT
  CREATED ......: 18-06-91, 02-02-93 pt: vain superit
  changePVM ....: 25.09.96 /tt --> Ruotsiksi, toimimaan nn:n kanssa
                  10.02.99 /pt --> in English
                  25.05.99 /jp --> uright1 & uright2 added
                  06.11.02 /jr Eventlog
                  08.11.02 /jr Changed null/copy to buffer-copy  
                  06.03.03 /tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TMSPrinter'}

{Syst/eventval.i} 

IF karyhma = "1" THEN DO:
   BELL.
   message "Sorry - You are not a superuser !".
   PAUSE 3 no-message.
   RETURN.
END.

DEF NEW shared VAR si-kirj LIKE TMSPrinter.PrinterId NO-UNDO.

def var ok as lo format "Yes/No" NO-UNDO.
DEF BUFFER mPrintCodes FOR PrintCodes.
DEF BUFFER mTMSPrinter FOR TMSPrinter.

DEF VAR memory          AS RECID.
def var line            as int format "99"    NO-UNDO.
DEF VAR delline         AS INT                NO-UNDO.
DEF VAR must-print      AS LOG                NO-UNDO.
DEF VAR must-add        AS LOG                NO-UNDO.
DEF VAR ufkey           AS LOG                NO-UNDO.
DEF VAR fr-header       AS CHAR.
DEF VAR rtab            AS RECID EXTENT 24    NO-UNDO.
DEF VAR i               AS INT                NO-UNDO.
DEF VAR xrecid          AS RECID.
DEF VAR PrinterId     LIKE TMSPrinter.PrinterId.
DEF VAR kopioitava      AS LOG                NO-UNDO.
DEF VAR ukirloo         LIKE PrinterId.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTMSPrinter AS HANDLE NO-UNDO.
   lhTMSPrinter = BUFFER TMSPrinter:HANDLE.
   RUN StarEventInitialize(lhTMSPrinter).

   DEFINE VARIABLE lhmTMSPrinter AS HANDLE NO-UNDO.
   lhmTMSPrinter = BUFFER mTMSPrinter:HANDLE.
   RUN StarEventInitialize(lhmTMSPrinter).

   DEFINE VARIABLE lhPrintCodes AS HANDLE NO-UNDO.
   lhPrintCodes = BUFFER PrintCodes:HANDLE.
   RUN StarEventInitialize(lhPrintCodes).

   DEFINE VARIABLE lhmPrintCodes AS HANDLE NO-UNDO.
   lhmPrintCodes = BUFFER mPrintCodes:HANDLE.
   RUN StarEventInitialize(lhmPrintCodes).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhTMSPrinter).
   END.
END.

form
   ukirloo label "Logical name ...."
   help "Printer's LOGICAL name, for user's eyes"
   WITH side-labels centered ROW 5 COLOR value(cfc) TITLE COLOR value(ctc)
   " COPY PRINTER " OVERLAY FRAME kopio.

form
    TMSPrinter.PrinterId  column-label "Logical name"
    help "Printer's LOGICAL Name, for user's eyes" 
    TMSPrinter.Device  column-label "Device"
    help "Printer's physical (device) name"        
WITH width 80 OVERLAY scroll 1 15 DOWN
    color value(cfc) title color value(ctc) ynimi + " PRINTERS "
    + string(pvm,"99-99-99")
    FRAME sel.

form
    TMSPrinter.PrinterId label "Logical name ...."
    help "Printer's LOGICAL name, for users' eyes"
    TMSPrinter.Device label "Device .........."
    help "Printer's physical (device) name" SKIP

    TMSPrinter.PageWidth  label "Page width ......"
    help "How many chars MAX are printable on one line" SKIP
    TMSPrinter.PageLength  label "Page length ....."
    help "How many lines MAX are printable on one row"
    TMSPrinter.PageAvail label " - use # of rows "
    help "Use this many rows" SKIP
    WITH  OVERLAY ROW 8 centered COLOR value(cfc)
    TITLE COLOR value(ctc) fr-header WITH side-labels
    FRAME lis.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.
FIND FIRST TMSPrinter no-lock no-error.
IF AVAILABLE TMSPrinter THEN DO:
   ASSIGN
   memory = recid(TMSPrinter)
   must-print = TRUE
   must-add    = FALSE
   kopioitava   = FALSE.
END.
ELSE DO:
   ASSIGN
   memory = ?
   must-print = FALSE
   must-add    = TRUE
   kopioitava   = FALSE.
END.
ASSIGN xrecid = ? delline = 0 ufkey = TRUE.

LOOP:
repeat WITH FRAME sel ON ENDKEY UNDO LOOP, NEXT LOOP:

   IF must-add THEN DO ON ENDKEY UNDO, NEXT:  /* TMSPrinter -ADD  */
      ASSIGN
      cfc = "lis"
      ufkey = TRUE
      fr-header = " ADD ".
      RUN Syst/ufcolor.p.
add-new:
      repeat WITH FRAME lis:
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.

         CREATE TMSPrinter.

         memory = recid(TMSPrinter).
         FIND TMSPrinter where recid(TMSPrinter) = memory exclusive-lock no-error.
         ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.

         UPDATE TMSPrinter.PrinterId
         VALIDATE
            (PrinterId = "" OR
            NOT can-find(TMSPrinter using  PrinterId),
            "Logical name " + string(input PrinterId) + 
            "already exists !").
         if input TMSPrinter.PrinterId = "" THEN UNDO, LEAVE add-new.

         ASSIGN 
         TMSPrinter.PrinterId = INPUT FRAME lis TMSPrinter.PrinterId.

         UPDATE 
         TMSPrinter.Device 
         TMSPrinter.PageWidth
         TMSPrinter.PageLength 
         TMSPrinter.PageAvail.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTMSPrinter).

         ASSIGN
         memory = recid(TMSPrinter)
         xrecid = memory.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST TMSPrinter no-lock no-error.
      IF NOT AVAILABLE TMSPrinter THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND TMSPrinter where recid(TMSPrinter) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         alkaen lineltA delline */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE TMSPrinter THEN DO:
               DISPLAY TMSPrinter.PrinterId TMSPrinter.Device.
               rtab[FRAME-LINE] = recid(TMSPrinter).
               FIND NEXT TMSPrinter no-lock no-error.
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
         ufk[1]= 161  
         ufk[2]= (IF lcRight = "RW" THEN 272 ELSE 0)
         ufk[3]= 0 ufk[4]= 0
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)   
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      CHOOSE ROW TMSPrinter.PrinterId {Syst/uchoose.i} no-error WITH FRAME sel.
      COLOR DISPLAY value(ccc) TMSPrinter.PrinterId WITH FRAME sel.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "Move upwards !".                  
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND TMSPrinter where recid(TMSPrinter) = rtab[1] no-lock.
            FIND prev TMSPrinter no-lock no-error.
            IF NOT AVAILABLE TMSPrinter THEN DO:
               message "THIS IS THE 1ST ROW !".        
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY TMSPrinter.PrinterId TMSPrinter.Device.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(TMSPrinter)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:

         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND TMSPrinter where recid(TMSPrinter) = rtab[FRAME-DOWN] no-lock .
            FIND NEXT TMSPrinter no-lock no-error.
            IF NOT AVAILABLE TMSPrinter THEN DO:
               message "THIS IS THE LAST PAGE !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* was found vielA seuraava tietue */
               scroll up.
               DISPLAY TMSPrinter.PrinterId TMSPrinter.Device.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(TMSPrinter).
               /* ja lopuksi pannaan memoryin ylimmAn linen avain */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
         memory = rtab[1].
         FIND TMSPrinter where recid(TMSPrinter) = memory no-lock no-error.
         FIND prev TMSPrinter no-lock no-error.

         IF AVAILABLE TMSPrinter THEN DO:
            memory = recid(TMSPrinter).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               FIND prev TMSPrinter no-lock no-error.
               IF AVAILABLE TMSPrinter THEN memory = recid(TMSPrinter).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "THIS IS THE 1ST PAGE !".          
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
            FIND TMSPrinter where recid(TMSPrinter) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     if lookup(nap,"1,f1") > 0 THEN DO:  /* Tehosteet */
        {Syst/uright2.i}
        FIND TMSPrinter where recid(TMSPrinter) = rtab[FRAME-LINE] no-lock.
        si-kirj = TMSPrinter.PrinterId.
        RUN Syst/ututeh.p.
        ufkey = TRUE.
        NEXT LOOP.
     END.
     else if lookup(nap,"2,f2") > 0 THEN DO:
          {Syst/uright2.i}

          FIND TMSPrinter where recid(TMSPrinter) = rtab[FRAME-LINE]
          no-lock no-error.
          PrinterId = TMSPrinter.PrinterId.
          cfc = "lis".  RUN Syst/ufcolor.p.
          DO WITH FRAME kopio:
             ehto=9. RUN Syst/ufkey.p. ufkey = TRUE.
             UPDATE 
             ukirloo validate(ukirloo = "" OR 
                              NOT can-find(TMSPrinter where 
                                           TMSPrinter.PrinterId
                                            = input ukirloo),
                      "That printer already exists !").

             if ukirloo ne "" THEN 
             DO:
                CREATE mTMSPrinter.
                BUFFER-COPY TMSPrinter EXCEPT TMSPrinter.PrinterId 
                TO mTMSPrinter.
                ASSIGN mTMSPrinter.PrinterId = ukirloo.
                IF llDoEvent 
                THEN RUN StarEventMakeCreateEvent(lhmTMSPrinter).
                FIND TMSPrinter NO-LOCK WHERE RECID(TMSPrinter) =
                                              RECID(mTMSPrinter).
                ASSIGN
                memory = recid(TMSPrinter)
                must-print = TRUE.


                /* kopioidaan tehosteet */
                FOR EACH PrintCodes where 
                         PrintCodes.PrinterId = PrinterId:
                   CREATE mPrintCodes.
                   BUFFER-COPY PrintCodes EXCEPT PrintCodes.PrinterId
                   TO mPrintCodes.
                   ASSIGN mPrintCodes.PrinterId = ukirloo.
                   IF llDoEvent 
                   THEN RUN StarEventMakeCreateEvent(lhmPrintCodes).

                END. /* tehosteiden FOR EACH */
             END.
             HIDE FRAME kopio no-pause.
         END.
         NEXT LOOP.
     END.
     else if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisAys */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" THEN DO:  /* removal */
        cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
        delline = FRAME-LINE.
        FIND TMSPrinter where recid(TMSPrinter) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) TMSPrinter.PrinterId TMSPrinter.Device.

        FIND NEXT TMSPrinter no-lock no-error.
        IF AVAILABLE TMSPrinter THEN memory = recid(TMSPrinter).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND TMSPrinter where recid(TMSPrinter) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           FIND prev TMSPrinter no-lock no-error.
           IF AVAILABLE TMSPrinter THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(TMSPrinter).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND TMSPrinter where recid(TMSPrinter) = rtab[FRAME-LINE] exclusive-lock.

        ASSIGN ok = FALSE.
        message "ARE YOU SURE YOU WANT TO DELETE (Y/N)? " UPDATE ok.
        COLOR DISPLAY value(ccc) TMSPrinter.PrinterId TMSPrinter.Device.
        IF ok THEN DO:
           IF can-find(FIRST TMSRepCfg where 
            TMSRepCfg.PrinterId = TMSPrinter.PrinterId)
           THEN DO:
              BELL.
              MESSAGE
              "This printer is used for printout definitions - do NOT delete !".
              PAUSE 1 no-message.
              HIDE MESSAGE no-pause.
              delline = 0.
              NEXT LOOP.
            END.
            FOR EACH PrintCodes where PrintCodes.PrinterId = TMSPrinter.PrinterId:             IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPrintCodes).
                DELETE PrintCodes.
            END.
            HIDE MESSAGE no-pause.

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMSPrinter).
            DELETE TMSPrinter.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST TMSPrinter) THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 THEN DO WITH FRAME lis:
        /* change */
        FIND TMSPrinter where recid(TMSPrinter) = rtab[frame-line(sel)]
        exclusive-lock.
        assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
        cfc = "lis". RUN Syst/ufcolor.p.
        DISPLAY 
           TMSPrinter.PrinterId
           TMSPrinter.Device 
           TMSPrinter.PageWidth 
           TMSPrinter.PageLength 
           TMSPrinter.PageAvail.

        IF lcRight = "RW" THEN DO:
           RUN Syst/ufkey.p.
           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMSPrinter).

           UPDATE 
              TMSPrinter.Device 
              TMSPrinter.PageWidth 
              TMSPrinter.PageLength 
              TMSPrinter.PageAvail.

           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMSPrinter).

        END.
        ELSE PAUSE.
        HIDE FRAME lis no-pause.
        DISPLAY TMSPrinter.Device  WITH FRAME sel.
        xrecid = recid(TMSPrinter).
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* LAST record */
        FIND LAST TMSPrinter no-lock.
        ASSIGN
        memory = recid(TMSPrinter)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"home") > 0 THEN DO:
        FIND FIRST TMSPrinter no-lock.
        ASSIGN
        memory = recid(TMSPrinter)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

