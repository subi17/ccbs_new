/* -----------------------------------------------
  MODULE .......: NNCUYP.P
  FUNCTION .....: Maintain CurRate codes
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 30-06-99
  MODIFIED .....: 29.04.02/tk eventlogging added
                  25.02.03/tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CurRate'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCurRate AS HANDLE NO-UNDO.
   lhCurRate = BUFFER CurRate:HANDLE.
   RUN StarEventInitialize(lhCurRate).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhCurRate).
   END.

END.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF BUFFER xcurr FOR CurRate.

DEF VAR Currency    LIKE CurRate.Currency  NO-UNDO.
DEF VAR RateDate    LIKE CurRate.RateDate NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 2.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR must-add   AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.

form
   CurRate.Currency
   CurRate.RateDate
   Currency.CurrName 
   CurRate.ExchRate
WITH width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " Maintain Currency Rates "
   + string(pvm,"99-99-99") + " "
   FRAME sel.

form
   CurRate.Currency
   CurRate.RateDate
   Currency.CurrName 
   CurRate.ExchRate
WITH  OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH side-labels 1 columns
   FRAME lis.

form /*  search WITH FIELD Currency */
    Currency
    help "Give ...."
    with row 4 col 2 title color value(ctc) " FIND Currency CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD RateDate */
    RateDate
    help "Give ..."
    with row 4 col 2 title color value(ctc) " FIND Currency Date "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST CurRate
/* search condition */ no-lock no-error.
IF AVAILABLE CurRate THEN ASSIGN
   memory     = recid(CurRate)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE 
         "No currency rates available !" SKIP
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory     = ?
      must-print = FALSE
      must-add   = TRUE.
END.
LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by code ".
       if order = 2 then put screen row 19 col 30 " Order by Date ".
    END.

   IF must-add THEN DO:  /* CurRate -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN ufkey.
        DO TRANSAction:
           CREATE CurRate.
           UPDATE 
              CurRate.Currency
              CurRate.RateDate
              CurRate.ExchRate
           WITH FRAME lis EDITING.
              READKEY. nap = keylabel(LASTKEY).
              if keylabel(lastkey) = "F4" THEN UNDO add-new, LEAVE add-new.
              IF lookup(nap,poisnap) > 0 THEN DO:
                 if frame-field = "currency" THEN DO:
                    if input frame lis CurRate.Currency = "" THEN
                       UNDO add-new, LEAVE add-new.

                    ASSIGN CurRate.Currency.

                    FIND FIRST Currency where
                               Currency.Currency = CurRate.Currency
                    no-lock no-error.

                    IF NOT AVAIL Currency THEN DO:
                       message "Currency " + CurRate.Currency + 
                               " does NOT exists. Add it first !"
                       VIEW-AS ALERT-BOX error.
                       UNDO,NEXT.
                    END.

                    FIND LAST xcurr where
                              xcurr.Currency = INPUT FRAME lis CurRate.Currency
                    no-lock no-error.
                    IF AVAIL xcurr THEN DO:
                       DISP
                          TODAY         @ CurRate.RateDate
                          xcurr.ExchRate @ CurRate.ExchRate
                          Currency.CurrName
                       WITH FRAME lis.
                    END.
                 END.
                 if frame-field = "RateDate" THEN DO:
                    FIND FIRST xcurr where
                               xcurr.Currency = INPUT FRAME lis
                                               CurRate.Currency AND
                               xcurr.Ratedate = INPUT FRAME lis
                                               CurRate.RateDate AND
                               recid(xcurr) NE recid(CurRate)
                    no-lock no-error.
                    IF AVAIL xcurr THEN DO:
                       BELL.
                       message "Rate for " + INPUT FRAME lis CurRate.Currency +
                               " already exists with this Date !".
                       NEXT.
                    END.
                 END.
              END.
              APPLY LASTKEY.
           END.
           ASSIGN
              memory = recid(CurRate)
              xrecid = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCurRate).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST CurRate
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE CurRate THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND CurRate where recid(CurRate) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE CurRate THEN DO:
              FIND FIRST Currency where
                         Currency.Currency = CurRate.Currency
              no-lock no-error.
              DISPLAY 
                 CurRate.Currency 
                 CurRate.RateDate
                 Currency.CurrName
                 CurRate.ExchRate.
              rtab[FRAME-LINE] = recid(CurRate).
              IF order = 1 THEN FIND NEXT CurRate
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT CurRate USE-INDEX RateDate
              /* search condition */ no-lock no-error.
           END.
           ELSE DO:
              CLEAR no-pause.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN firstline.
        ASSIGN firstline = 0
               must-print = FALSE.
        PAUSE 0 no-message.

        /* one page of data has been Printed AND
        the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (if lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (if lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW CurRate.Currency ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CurRate.Currency WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CurRate.RateDate ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CurRate.RateDate WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND CurRate where recid(CurRate) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev CurRate
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev CurRate USE-INDEX RateDate
           /* search condition */ no-lock no-error.
           IF AVAILABLE CurRate THEN
              ASSIGN firstline = i memory = recid(CurRate).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        message "You are on a empty row, move upwards !".
        PAUSE 1 no-message.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND CurRate where recid(CurRate) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev CurRate
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev CurRate USE-INDEX RateDate
           /* search condition */ no-lock no-error.
           IF NOT AVAILABLE CurRate THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              FIND FIRST Currency where
                         Currency.Currency = CurRate.Currency
              no-lock no-error.
              DISPLAY 
                 CurRate.Currency 
                 CurRate.RateDate
                 Currency.CurrName
                 CurRate.ExchRate.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(CurRate)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND CurRate where recid(CurRate) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT CurRate
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT CurRate USE-INDEX RateDate
           /* search condition */ no-lock no-error.
           IF NOT AVAILABLE CurRate THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              FIND FIRST Currency where
                         Currency.Currency = CurRate.Currency
              no-lock no-error.
              DISPLAY 
                 CurRate.Currency 
                 CurRate.RateDate
                 Currency.CurrName
                 CurRate.ExchRate.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(CurRate).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND CurRate where recid(CurRate) = memory no-lock no-error.
        IF order = 1 THEN FIND prev CurRate
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev CurRate USE-INDEX RateDate
        /* search condition */ no-lock no-error.
        IF AVAILABLE CurRate THEN DO:
           memory = recid(CurRate).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev CurRate
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev CurRate USE-INDEX RateDate
              /* search condition */ no-lock no-error.
              IF AVAILABLE CurRate THEN memory = recid(CurRate).
              ELSE line = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* this is the FIRST data page */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           memory = rtab[FRAME-DOWN].
           FIND CurRate where recid(CurRate) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       Currency = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE Currency WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if Currency <> "" THEN DO:
          FIND FIRST CurRate where CurRate.Currency >= Currency
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE CurRate THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  currate/cu-code was found */
          ASSIGN order = 1 memory = recid(CurRate) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       RateDate = ?.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE RateDate WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       IF RateDate <> ? THEN DO:
          FIND FIRST CurRate where CurRate.RateDate >= RateDate
          USE-INDEX RateDate /* search condition */ no-lock no-error.
          IF NOT AVAILABLE CurRate THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  currate/cu-date was found */
          ASSIGN order = 2 memory = recid(CurRate) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 and lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND CurRate where recid(CurRate) = rtab[FRAME-LINE] no-lock.

       FIND FIRST Currency where
                  Currency.Currency = CurRate.Currency
       no-lock no-error.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          CurRate.Currency 
          CurRate.RateDate 
          Currency.CurrName 
          CurRate.ExchRate.

       IF order = 1 THEN FIND NEXT CurRate
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT CurRate USE-INDEX RateDate
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND NEXT CurRate USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT CurRate USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE CurRate THEN memory = recid(CurRate).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND CurRate where recid(CurRate) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev CurRate
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev CurRate USE-INDEX RateDate
          /* search condition */ no-lock no-error.
          IF AVAILABLE CurRate THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(CurRate).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND CurRate where recid(CurRate) = rtab[FRAME-LINE]
       exclusive-lock.

       FIND FIRST Currency where
                  Currency.Currency = CurRate.Currency
       no-lock no-error.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          CurRate.Currency 
          CurRate.RateDate 
          Currency.CurrName 
          CurRate.ExchRate.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCurRate).

           DELETE CurRate.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST CurRate
           /* search condition */) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
       /* change */
       FIND CurRate where recid(CurRate) = rtab[frame-line(sel)]
       exclusive-lock.

       FIND FIRST Currency where
                  Currency.Currency = CurRate.Currency
       no-lock no-error.

       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       cfc = "lis". RUN ufcolor.
       DISPLAY 
          CurRate.Currency
          Currency.CurrName
          CurRate.RateDate
          CurRate.ExchRate.


       IF lcRight = "RW" THEN DO:
          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCurRate).
          UPDATE CurRate.ExchRate. 
          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCurRate).
       END.
       ELSE pause.
       HIDE FRAME lis no-pause.
       DISPLAY 
          CurRate.RateDate
          CurRate.ExchRate
       WITH FRAME sel.
       xrecid = recid(CurRate).


     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST CurRate
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST CurRate USE-INDEX RateDate
       /* search condition */ no-lock no-error.
       ASSIGN memory = recid(CurRate) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST CurRate
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST CurRate USE-INDEX RateDate
       /* search condition */ no-lock no-error.
       ASSIGN memory = recid(CurRate) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

