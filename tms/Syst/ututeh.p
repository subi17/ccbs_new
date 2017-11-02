/* -----------------------------------------------
  MODULE .......: UTUTEH.P
  FUNCTION .....: Kirjoitintehosteiden yllApito
  APPLICATION ..: TS
  AUTHOR .......: TT
  CREATED ......: 17.06.1991
  changePVM ....: 03.04.92/tt
                  19.09.96 /tt Tekstit kaannetty ruotsiksi
                  25.06.98 /kl vast => ok
                  14.04.99 /pt in English
                  11.11.02 /jr Eventlog
                  06.03.03 /tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PrintCodes'}

DEF  shared VAR si-kirj LIKE TMSPrinter.PrinterId NO-UNDO.

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

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPrintCodes AS HANDLE NO-UNDO.
   lhPrintCodes = BUFFER PrintCodes:HANDLE.
   RUN StarEventInitialize(lhPrintCodes).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhPrintCodes).
   END.
END.

form
    PrintCodes.Effect   column-label "Code"
    help "Unique code of an Printer Effect"
    EffName          column-label "Name Of Effect"
    help "Name/Purpose"
    PrintCodes.PageWidth  column-label "Max Width"
    PrintCodes.PageLength  column-label "Lines/Page"
    PrintCodes.AvailLines column-label "Use Lines"
    WITH ROW 2 col 2 centered OVERLAY scroll 1 13 DOWN
    COLOR value(Syst.Var:cfc)
    TITLE COLOR value(Syst.Var:ctc)
    " PRINTER '" + si-kirj + "':  EFFECTS "
    FRAME sel.

form
    PrintCodes.Effect   label "Code ...." AT 2
    help "Code of an Effect, (one digit)" SKIP
    EffName          label "Name ...." AT 2
    help "Name of an effect" SKIP
    PrintCode.EffOn[1]        label "Start ..." AT 2
    help "Code sequence for START" SKIP
    PrintCode.EffOff[1]       label "End ....." AT 2
    help "Code Sequence for END" SKIP
    PrintCodes.PageWidth  label "Max.width" AT 2
    help "Maximum printout width (char/line) when using this effect" SKIP
    PrintCodes.PageLength  label "Lin/Page " AT 2
    help "Maximum number of lines/page when using this effect" SKIP
    PrintCodes.AvailLines label "Use lines" AT 2
    help "How many lines shall be Printed on one page before FORM FEED" SKIP
    WITH OVERLAY ROW 8 centered
    COLOR value(Syst.Var:cfc)
    TITLE COLOR value(Syst.Var:ctc)
    fr-header side-labels 
    FRAME lis.

form
    PrintCode.EffOn[2]      label "Start" AT 2 SKIP
    PrintCode.EffOff[2]     label "End  " AT 2 SKIP
    WITH OVERLAY ROW 15 width 78
    COLOR value(Syst.Var:cfc)
    TITLE COLOR value(Syst.Var:ctc)
    " Effect " + PrintCodes.Effect + " INTERPRETED " side-labels centered
    FRAME nakym.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
view FRAME sel.
FIND FIRST TMSPrinter where TMSPrinter.PrinterId = si-kirj no-lock no-error.
FIND FIRST PrintCodes where PrintCodes.PrinterId = si-kirj no-lock no-error.
IF AVAILABLE PrintCodes THEN DO:
   ASSIGN
   memory = recid(PrintCodes)
   must-print = TRUE
   must-add    = FALSE.
END.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No printer effects available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.   
   ELSE ASSIGN
      memory = ?
      must-print = FALSE
      must-add    = TRUE.
END.
ASSIGN xrecid = ? delline = 0 ufkey = TRUE.

LOOP:
repeat WITH FRAME sel ON ENDKEY UNDO LOOP, NEXT LOOP:

   IF must-add THEN DO:  /* PrintCodes -ADD  */
      ASSIGN
      Syst.Var:cfc = "lis"
      ufkey = TRUE
      fr-header = " ADD A NEW RECORD ".

      RUN Syst/ufcolor.p.
add-new:
      repeat WITH FRAME lis:
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         Syst.Var:ehto = 9. RUN Syst/ufkey.p.
         PROMPT-FOR PrintCodes.Effect.
         if input PrintCodes.Effect = "" THEN LEAVE add-new.

         IF can-find(PrintCodes where PrintCodes.Effect =
         INPUT Effect AND PrintCodes.PrinterId = si-kirj) THEN DO:
            BELL.
            message "EFFECT " + string(INPUT PrintCodes.Effect) + 
            " already exists !".
            PROMPT-FOR PrintCodes.Effect. NEXT.
         END.
         CREATE PrintCodes.

         ASSIGN PrintCodes.PrinterId = si-kirj
                PrintCodes.Effect   = caps(INPUT FRAME lis PrintCodes.Effect)
                PrintCodes.PageWidth  = TMSPrinter.PageWidth
                PrintCodes.PageLength  = TMSPrinter.PageLength
                PrintCodes.AvailLines = TMSPrinter.PageAvail.
         UPDATE EffName PrintCode.EffOn[1] PrintCode.EffOff[1]
                PrintCodes.PageWidth PrintCodes.PageLength PrintCodes.AvailLines.
         ASSIGN
         memory = recid(PrintCodes)
         xrecid = memory.

         /* PrintCode.EffOnA2A laskeminen */

         DO idx = 1 TO length(PrintCode.EffOn[1]):
            ASSIGN teho = substring(PrintCode.EffOn[1],idx,1).
            IF teho = chr(092) OR teho = chr(153) THEN DO:
               if index("0123456789",substring(PrintCode.EffOn[1],idx + 1,1)) 
               GT 0 THEN
                  ASSIGN teho = chr(integer(substring(PrintCode.EffOn[1],idx + 1,3)))
                  idx = idx + 3.
               ELSE ASSIGN teho = substring(PrintCode.EffOn[1],idx + 1,1)
                  idx = idx + 1.
            END.
            ASSIGN PrintCode.EffOn[2] = PrintCode.EffOn[2] + teho.
         END.

         /* PrintCode.EffOffA2A laskeminen */

         DO idx = 1 TO length(PrintCode.EffOff[1]):
            ASSIGN teho = substring(PrintCode.EffOff[1],idx,1).
            IF teho = chr(092) OR teho = chr(153) THEN DO:
               if index("0123456789",substring(PrintCode.EffOff[1],idx + 1,1)) GT 0 THEN
                  ASSIGN teho = chr(integer(substring(PrintCode.EffOff[1],idx + 1,3)))
                  idx = idx + 3.
               ELSE ASSIGN teho = substring(PrintCode.EffOff[1],idx + 1,1)
                  idx = idx + 1.
            END.
            ASSIGN PrintCode.EffOff[2] = PrintCode.EffOff[2] + teho.
         END.
         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPrintCodes).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST PrintCodes where PrintCodes.PrinterId = si-kirj no-lock no-error.
      IF NOT AVAILABLE PrintCodes THEN LEAVE LOOP.

      NEXT LOOP.
   END.

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
               FIND NEXT PrintCodes where PrintCodes.PrinterId = si-kirj
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
         Syst.Var:ufk[1]= 128 Syst.Var:ufk[2]= 150  Syst.Var:ufk[3]= 0 Syst.Var:ufk[4]= 0
         Syst.Var:ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
         Syst.Var:ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)   
         Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
         Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      CHOOSE ROW PrintCodes.EffName {Syst/uchoose.i} no-error WITH FRAME sel.
      COLOR DISPLAY normal PrintCodes.EffName WITH FRAME sel.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "YOU ARE EN AN EMPTY ROW, MOVE UPWARDS !".
         PAUSE 1 no-message.
         NEXT.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND PrintCodes where recid(PrintCodes) = rtab[1] no-lock.
            FIND prev PrintCodes where PrintCodes.PrinterId = si-kirj 
            no-lock no-error.
            IF NOT AVAILABLE PrintCodes THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
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
      else if lookup(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:

         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND PrintCodes where recid(PrintCodes) = rtab[FRAME-DOWN] no-lock .
            FIND NEXT PrintCodes where PrintCodes.PrinterId = si-kirj no-lock no-error.
            IF NOT AVAILABLE PrintCodes THEN DO:
               message "YOU ARE ON THE LAST ROW !".
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
      else if lookup(Syst.Var:nap,"prev-page,page-up") > 0 THEN DO:
         memory = rtab[1].
         FIND PrintCodes where recid(PrintCodes) = memory no-lock no-error.
         FIND prev PrintCodes where PrintCodes.PrinterId = si-kirj no-lock no-error.

         IF AVAILABLE PrintCodes THEN DO:
            memory = recid(PrintCodes).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               FIND prev PrintCodes where PrintCodes.PrinterId = si-kirj
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
     else if lookup(Syst.Var:nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:

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

     if lookup(Syst.Var:nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"1,f1") > 0 THEN DO:  /* koeprint-line */
          RUN Syst/nnthtu.p.
          ufkey = TRUE.
          NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"2,f2") > 0 THEN DO:  /* nAkymAtOn toiminto */
          /* PrintCode.EffOnA2A & PrintCode.EffOffA2A print-line apuframelle. */
          PAUSE 0 no-message.
          FIND PrintCodes where recid(PrintCodes) = rtab[frame-line(sel)] no-lock.
          Syst.Var:cfc = "lis". RUN Syst/ufcolor.p.
          DISPLAY PrintCode.EffOn[2] PrintCode.EffOff[2] WITH FRAME nakym.
          pause message "Press Entrer !".               
          HIDE FRAME nakym.
     END.

     else if lookup(Syst.Var:nap,"6,f6") > 0 AND lcRight = "RW" THEN DO:  /* removal */
        Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
        delline = FRAME-LINE.
        FIND PrintCodes where recid(PrintCodes) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(Syst.Var:ctc) PrintCodes.Effect EffName
                       PrintCodes.PageWidth PrintCodes.PageLength PrintCodes.AvailLines.

        FIND NEXT PrintCodes where PrintCodes.PrinterId = si-kirj no-lock no-error.
        IF AVAILABLE PrintCodes THEN memory = recid(PrintCodes).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND PrintCodes where recid(PrintCodes) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           FIND prev PrintCodes where PrintCodes.PrinterId = si-kirj no-lock no-error.
           IF AVAILABLE PrintCodes THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(PrintCodes).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND PrintCodes where recid(PrintCodes) = rtab[FRAME-LINE] exclusive-lock.

        ASSIGN ok = FALSE.
        message "ARE YOU SURE YOU WANT TO REMOVE (Y/N)? " UPDATE ok.
        COLOR DISPLAY value(Syst.Var:ccc) PrintCodes.Effect EffName
                       PrintCodes.PageWidth PrintCodes.PageLength PrintCodes.AvailLines.
        IF ok THEN DO:
            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPrintCodes).
            DELETE PrintCodes.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST PrintCodes where PrintCodes.PrinterId = si-kirj)
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

     else if lookup(Syst.Var:nap,"enter,return") > 0 THEN DO WITH FRAME lis: /* change */

        FIND PrintCodes where recid(PrintCodes) = rtab[frame-line(sel)]
        exclusive-lock.
        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPrintCodes).
        ASSIGN
        fr-header = " CHANGE " ufkey = TRUE Syst.Var:ehto = 9.
        RUN Syst/ufkey.p.
        Syst.Var:cfc = "lis". RUN Syst/ufcolor.p.
        DISPLAY 
            PrintCodes.Effect
            EffName 
            PrintCode.EffOn[1] 
            PrintCode.EffOff[1]
            PrintCodes.PageWidth 
            PrintCodes.PageLength 
            PrintCodes.AvailLines.

        IF lcRight = "RW" THEN DO:

           UPDATE 
               EffName 
               PrintCode.EffOn[1] 
               PrintCode.EffOff[1]
               PrintCodes.PageWidth 
               PrintCodes.PageLength 
               PrintCodes.AvailLines.
           assign 
               PrintCode.EffOn[2] = "" PrintCode.EffOff[2] = "".

        /* PrintCode.EffOnA2A laskeminen */

           DO idx = 1 TO length(PrintCode.EffOn[1]):
              ASSIGN teho = substring(PrintCode.EffOn[1],idx,1).
              IF teho = chr(092) OR teho = chr(153) THEN DO:
                 if index("0123456789", substring(PrintCode.EffOn[1],
                                                  idx + 1,1)) GT 0 
                 THEN ASSIGN 
                   teho = chr(integer(substring(PrintCode.EffOn[1],idx + 1,3)))
                   idx = idx + 3.
                 ELSE ASSIGN 
                   teho = substring(PrintCode.EffOn[1],idx + 1,1)
                   idx = idx + 1.
              END.
              ASSIGN PrintCode.EffOn[2] = PrintCode.EffOn[2] + teho.
           END.

           /* PrintCode.EffOffA2A laskeminen */

           DO idx = 1 TO length(PrintCode.EffOff[1]):
              ASSIGN teho = substring(PrintCode.EffOff[1],idx,1).
              IF teho = chr(092) OR teho = chr(153) THEN DO:
                 if index("0123456789", substring(PrintCode.EffOff[1],
                                                  idx + 1,1)) GT 0 
                 THEN ASSIGN 
                    teho = chr(integer(substring(
                           PrintCode.EffOff[1],idx + 1,3)))
                    idx = idx + 3.
                 ELSE ASSIGN 
                    teho = substring(PrintCode.EffOff[1],idx + 1,1)
                    idx = idx + 1.
              END.
              ASSIGN PrintCode.EffOff[2] = PrintCode.EffOff[2] + teho.
           END.

           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPrintCodes).

        END.
        ELSE PAUSE.
        HIDE FRAME lis no-pause.
        DISPLAY EffName PrintCodes.PageWidth PrintCodes.PageLength PrintCodes.AvailLines WITH FRAME sel.
        xrecid = recid(PrintCodes).

     END.

     else if lookup(Syst.Var:nap,"end") > 0 THEN DO : /* LAST record */
        FIND LAST PrintCodes where PrintCodes.PrinterId = si-kirj no-lock.
        ASSIGN
        memory = recid(PrintCodes)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"home") > 0 THEN DO:
        FIND FIRST PrintCodes where PrintCodes.PrinterId = si-kirj no-lock.
        ASSIGN
        memory = recid(PrintCodes)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
Syst.Var:si-recid = xrecid.

