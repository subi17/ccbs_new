/* -----------------------------------------------
  MODULE .......: NNCUYP.P
  FUNCTION .....: Maintain Currency codes
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 30-06-99

  MODIFIED .....: 26.04.02/tk - eventlogging added
                  11.09.02/jp - validation added
                  25.02.03/tk - tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'currency'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCurrency AS HANDLE NO-UNDO.
   lhCurrency = BUFFER Currency:HANDLE.
   RUN StarEventInitialize(lhCurrency).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCurrency).
   END.

END.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF BUFFER xcurr FOR Currency.

DEF VAR Currency    LIKE Currency.Currency  NO-UNDO.
DEF VAR CurrName    LIKE Currency.Currname  NO-UNDO.
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
   Currency.Currency
   Currency.CurrName 
   Currency.SubUnit
   Currency.SubName
WITH width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " Maintain Currency Codes "
   + string(pvm,"99-99-99") + " "
   FRAME sel.

form
   Currency.Currency
   VALIDATE(NOT CAN-FIND(currency WHERE currency.currency = input currency),
   "Currency already exists!")
   Currency.CurrName 
   VALIDATE(Currency.CurrName ne "","Missing currency name!")
   Currency.SubUnit
   VALIDATE(Currency.SubUnit ne "","Invalid SubUnit!")
   Currency.SubName
WITH  OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH side-labels 1 columns
   FRAME lis.

form /*  search WITH FIELD Currency */
    Currency
    help "Give ...."
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD CurrName */
    CurrName
    help "Give ..."
    with row 4 col 2 title color value(ctc) " FIND NAME "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST Currency
/* search condition */ no-lock no-error.
IF AVAILABLE Currency THEN ASSIGN
   memory     = recid(Currency)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No currencies available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Currency -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:
           CREATE Currency.
           UPDATE 
              Currency.Currency
              Currency.CurrName
              Currency.SubUnit
              Currency.SubName
           WITH FRAME lis EDITING.
              READKEY. nap = keylabel(LASTKEY).
              if keylabel(lastkey) = "F4" THEN UNDO add-new, LEAVE add-new.
              IF lookup(nap,poisnap) > 0 THEN DO:
                 if frame-field = "Currency" THEN DO:
                    if input frame lis Currency.Currency = "" THEN
                       UNDO add-new, LEAVE add-new.
                 END.
              END.
              APPLY LASTKEY.
           END.
           ASSIGN
              memory = recid(Currency)
              xrecid = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCurrency).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST Currency
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE Currency THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND Currency where recid(Currency) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE Currency THEN DO:
              DISPLAY 
                 Currency.Currency 
                 Currency.CurrName
                 Currency.SubUnit
                 Currency.SubName.
              rtab[FRAME-LINE] = recid(Currency).
              IF order = 1 THEN FIND NEXT Currency
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT Currency USE-INDEX CurrName
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
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW Currency.Currency {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Currency.Currency WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Currency.CurrName {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Currency.CurrName WITH FRAME sel.
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
        FIND Currency where recid(Currency) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev Currency
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev Currency USE-INDEX CurrName
           /* search condition */ no-lock no-error.
           IF AVAILABLE Currency THEN
              ASSIGN firstline = i memory = recid(Currency).
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
           FIND Currency where recid(Currency) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev Currency
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev Currency USE-INDEX CurrName
           /* search condition */ no-lock no-error.
           IF NOT AVAILABLE Currency THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISPLAY 
                 Currency.Currency 
                 Currency.CurrName
                 Currency.SubUnit
                 Currency.SubName.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(Currency)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND Currency where recid(Currency) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT Currency
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT Currency USE-INDEX CurrName
           /* search condition */ no-lock no-error.
           IF NOT AVAILABLE Currency THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISPLAY 
                 Currency.Currency 
                 Currency.CurrName
                 Currency.SubUnit
                 Currency.SubName.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(Currency).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Currency where recid(Currency) = memory no-lock no-error.
        IF order = 1 THEN FIND prev Currency
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev Currency USE-INDEX CurrName
        /* search condition */ no-lock no-error.
        IF AVAILABLE Currency THEN DO:
           memory = recid(Currency).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev Currency
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev Currency USE-INDEX CurrName
              /* search condition */ no-lock no-error.
              IF AVAILABLE Currency THEN memory = recid(Currency).
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
           FIND Currency where recid(Currency) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       Currency = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE Currency WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if Currency <> "" THEN DO:
          FIND FIRST Currency where Currency.Currency >= Currency
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE Currency THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  currency/cu-code was found */
          ASSIGN order = 1 memory = recid(Currency) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       CurrName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE CurrName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if CurrName <> "" THEN DO:
          FIND FIRST Currency where Currency.CurrName >= CurrName
          USE-INDEX CurrName /* search condition */ no-lock no-error.
          IF NOT AVAILABLE Currency THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  currency/cu-date was found */
          ASSIGN order = 2 memory = recid(Currency) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND Currency where recid(Currency) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          Currency.Currency 
          Currency.CurrName
          Currency.SubUnit
          Currency.SubName.

       IF order = 1 THEN FIND NEXT Currency
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT Currency USE-INDEX CurrName
       /* search condition */ no-lock no-error.
       IF AVAILABLE Currency THEN memory = recid(Currency).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND Currency where recid(Currency) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev Currency
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev Currency USE-INDEX CurrName
          /* search condition */ no-lock no-error.
          IF AVAILABLE Currency THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(Currency).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND Currency where recid(Currency) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          Currency.Currency 
          Currency.CurrName
          Currency.SubUnit
          Currency.SubName.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCurrency).

           DELETE Currency.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST Currency
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

     DO WITH FRAME lis transaction:
       /* change */
       FIND Currency where recid(Currency) = rtab[frame-line(sel)]
       exclusive-lock.

       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       DISPLAY 
          Currency.Currency
          Currency.CurrName
          Currency.SubUnit
          Currency.SubName.

       IF lcRight = "RW" THEN DO:
          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCurrency).

          UPDATE   
             Currency.CurrName
             Currency.SubUnit
             Currency.SubName.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCurrency).
       END.
       ELSE pause.
       HIDE FRAME lis no-pause.
       DISPLAY 
          Currency.CurrName
          Currency.SubUnit
          Currency.SubName
       WITH FRAME sel.
       xrecid = recid(Currency).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST Currency
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST Currency USE-INDEX CurrName
       /* search condition */ no-lock no-error.
       ASSIGN memory = recid(Currency) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST Currency
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST Currency USE-INDEX CurrName
       /* search condition */ no-lock no-error.
       ASSIGN memory = recid(Currency) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

