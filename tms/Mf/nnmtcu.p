/* -----------------------------------------------
  MODULE .......: NNMTCU.P
  FUNCTION .....: Maintain monthly Calls per customer
  APPLICATION ..: nn
  AUTHOR .......: kal
  CREATED ......: 22-12-97
  MODIFIED .....: 29-10-98 kl layout
                  03.03.03 tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'mthcall'}

DEF /* NEW */ shared VAR siirto AS CHAR.


DEF INPUT PARAM CustNum LIKE MthCall.CustNum   NO-UNDO.


DEF VAR seek-cust-nr LIKE MthCall.CustNum   NO-UNDO.
DEF VAR seek-mth     LIKE MthCall.Month       NO-UNDO.
DEF VAR Month          LIKE MthCall.Month       NO-UNDO.
DEF VAR Limit        LIKE MthCall.Limit     NO-UNDO.
DEF VAR CloseDate       LIKE MthCall.CloseDate    NO-UNDO.
DEF VAR Called       LIKE MthCall.Called    NO-UNDO.
DEF VAR xrecid       AS RECID                        init ?.
DEF VAR firstline    AS INT                 NO-UNDO  init 0.
DEF VAR order        AS INT                 NO-UNDO  init 1.
DEF VAR ordercount   AS INT                 NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                 NO-UNDO  init TRUE.
DEF VAR delline      AS INT                 NO-UNDO  init 0.
DEF VAR ex-order     AS INT                 NO-UNDO.
DEF VAR memory       AS RECID               NO-UNDO.
def var line         as int format "99"     NO-UNDO.
DEF VAR must-print   AS LOG                 NO-UNDO.
DEF VAR must-add     AS LOG                 NO-UNDO.
DEF VAR fr-header    AS CHAR                NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24     NO-UNDO.
DEF VAR i            AS INT                 NO-UNDO.
def var ok           as log format "Yes/No" NO-UNDO.


FIND FIRST Customer
where Customer.CustNum = CustNum no-lock no-error.

form
   MthCall.CustNum        label "Cust.nr"
   MthCall.Month            label "Month"       format "999999"
   MthCall.Called         label "Called"  
   MthCall.Limit          label "Limit"
   MthCall.CloseDate         label "Closed"      format "99-99-99"
WITH
   width 48 OVERLAY scroll 1 ROW 2 12 DOWN
   centered NO-LABEL COLOR value(cfc)
   title color value(ctc) " Monthly call counters "
   FRAME sel.

form
    "Customer number ..:" CustNum
       help "Existing customer number"                            SKIP
    "Month ............:" Month
       help "Month of limitation (YYYYMM)"                        SKIP
    "Called so far ....:" Called
       help "Value of Calls so far"                               SKIP
    "Mothly Limit .....:" Limit
       help "Limit of calls"                                      SKIP
    "Closing Date .....:" CloseDate
       help "Date when subscription was closed"                   SKIP

    WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH NO-LABEL
    FRAME lis.

FIND FIRST MthCall
where MthCall.CustNum = CustNum no-lock no-error.

IF AVAILABLE MthCall THEN ASSIGN
   memory       = recid(MthCall)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   BELL.
   message "This Customer Has No Monthly Counters - press ENTER !".
   PAUSE no-message.
   RETURN.
END.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       /*
       if order = 1 then put screen row 19 col 30 " Order by customer number ".
       if order = 2 then put screen row 19 col 30 " Order by month           ".
       */
    END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND MthCall where recid(MthCall) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE MthCall THEN DO:
              DISPLAY MthCall.CustNum MthCall.Month
                 MthCall.Called MthCall.Limit MthCall.CloseDate.
              rtab[FRAME-LINE] = recid(MthCall).
              IF order = 1 THEN FIND NEXT MthCall
              where MthCall.CustNum = CustNum no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT MthCall USE-INDEX Month
              where MthCall.CustNum = CustNum no-lock no-error.
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
        ufk[1] = 0  ufk[2] = 0 ufk[3] = 0 ufk[4] = 0
        ufk[5] = 0  ufk[6] = 0 ufk[7] = 0 ufk[8] = 8 ufk[9] = 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW MthCall.CustNum ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MthCall.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MthCall.Month ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MthCall.Month WITH FRAME sel.
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
        FIND MthCall where recid(MthCall) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev MthCall
           where MthCall.CustNum = CustNum no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
           where MthCall.CustNum = CustNum no-lock no-error.
           IF AVAILABLE MthCall THEN
              ASSIGN firstline = i memory = recid(MthCall).
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
           FIND MthCall where recid(MthCall) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev MthCall
           where MthCall.CustNum = CustNum no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
           where MthCall.CustNum = CustNum no-lock no-error.
           IF NOT AVAILABLE MthCall THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISPLAY MthCall.CustNum MthCall.Month
                      MthCall.Called MthCall.Limit MthCall.CloseDate.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(MthCall)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND MthCall where recid(MthCall) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT MthCall
           where MthCall.CustNum = CustNum no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT MthCall USE-INDEX Month
           where MthCall.CustNum = CustNum no-lock no-error.
           IF NOT AVAILABLE MthCall THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISPLAY MthCall.CustNum MthCall.Month
                      MthCall.Called MthCall.Limit MthCall.CloseDate.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(MthCall).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND MthCall where recid(MthCall) = memory no-lock no-error.
        IF order = 1 THEN FIND prev MthCall
        where MthCall.CustNum = CustNum no-lock no-error.
        ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
        where MthCall.CustNum = CustNum no-lock no-error.
        IF AVAILABLE MthCall THEN DO:
           memory = recid(MthCall).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev MthCall
              where MthCall.CustNum = CustNum no-lock no-error.
              ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
              where MthCall.CustNum = CustNum no-lock no-error.
              IF AVAILABLE MthCall THEN memory = recid(MthCall).
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
           FIND MthCall where recid(MthCall) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

/* FUNCTIONS DISABLED FROM HERE ...... 
IF THESE ARE TAKEN BACK TO USE THEN ADD EVENTLOG

     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND MthCall where recid(MthCall) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
       MthCall.CustNum MthCall.Month MthCall.Called MthCall.Limit MthCall.CloseDate.

       IF order = 1 THEN FIND NEXT MthCall
       where MthCall.CustNum = CustNum no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT MthCall USE-INDEX Month
       where MthCall.CustNum = CustNum no-lock no-error.
       IF AVAILABLE MthCall THEN memory = recid(MthCall).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND MthCall where recid(MthCall) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev MthCall
          where MthCall.CustNum = CustNum no-lock no-error.
          ELSE IF order = 2 THEN FIND prev MthCall USE-INDEX Month
          where MthCall.CustNum = CustNum no-lock no-error.
          IF AVAILABLE MthCall THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(MthCall).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND MthCall where recid(MthCall) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       MthCall.CustNum MthCall.Month MthCall.Called MthCall.Limit MthCall.CloseDate.
       IF ok THEN DO:

           DELETE MthCall.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST MthCall
           where MthCall.CustNum = CustNum) THEN DO:
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
       HIDE FRAME lis.
       FIND MthCall where recid(MthCall) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       cfc = "lis". RUN ufcolor.
       ASSIGN
          Month     = MthCall.Month
          Called  = MthCall.Called
          Limit   = MthCall.Limit
          CloseDate  = MthCall.CloseDate.
       DISPLAY CustNum Month Called.
       UPDATE Limit CloseDate.
       ASSIGN
          MthCall.Limit  = Limit
          MthCall.CloseDate = CloseDate.
       HIDE FRAME lis no-pause.
       DISPLAY MthCall.Limit MthCall.CloseDate
       WITH FRAME sel.
       xrecid = recid(MthCall).
     END.
....  DOWN TO HERE */

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST MthCall
       where MthCall.CustNum = CustNum no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST MthCall USE-INDEX Month
       where MthCall.CustNum = CustNum no-lock no-error.
       ASSIGN memory = recid(MthCall) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST MthCall
       where MthCall.CustNum = CustNum no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST MthCall USE-INDEX Month
       where MthCall.CustNum = CustNum no-lock no-error.
       ASSIGN memory = recid(MthCall) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

