/* -----------------------------------------------
  MODULE .......: NNCCYP.P
  FUNCTION .....: Maintain closed customers
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 29-12-97
  MODIFIED .....: 11.06.98 - kl
                  17.01.00 pt RETURN IF File is empty
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF STREAM whitelist.

DEF VAR CustNum    LIKE ClosedCust.CustNum   NO-UNDO.
DEF VAR Date       LIKE ClosedCust.Date      NO-UNDO.
DEF VAR DateOpen   LIKE ClosedCust.DateOpen  NO-UNDO.
DEF VAR cust-name  LIKE Customer.CustName    NO-UNDO.
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

DEF VAR Month        AS i  NO-UNDO.
def var whitelist  as c  no-undo format "x(40)".

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   whitelist = TMSUser.RepDir + "/".
END.

form
    ClosedCust.CustNum
    cust-name
    ClosedCust.Date
    ClosedCust.DateOpen
    ClosedCust.State     format "!/"
    ClosedCust.Printed   format "*/"
    ClosedCust.Called
    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Maintain closed customers "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ClosedCust.CustNum
    ClosedCust.Date
    ClosedCust.DateOpen
    ClosedCust.State
    WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 1 columns
    FRAME lis.

form /*  search WITH FIELD CustNum */
    CustNum
    help "Give customer number"
    with row 4 col 2 title color value(ctc) " FIND CUSTOMER "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

form /*  search WITH FIELD  */
    Date
    help "Give closing date"
    with row 4 col 2 title color value(ctc) " FIND CLOSING Date "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f2.

form /* UPDATE whitelist FileName */
    skip(1)
    "   Date:" DateOpen 
       help "Customers opened on this Date (empty for all)" 
    SKIP
    "   File:" whitelist
       help "Filename of the whitelist file"
    skip(1)
    WITH ROW 8 centered width 55
    title color value(cfc) " ADD OPENED NUMBERS TO A File "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME wl.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

ASSIGN
   Month    = (year(pvm) * 100) + month(pvm).

FIND FIRST ClosedCust
/* search condition */ no-lock no-error.
IF AVAILABLE ClosedCust THEN ASSIGN
   memory     = recid(ClosedCust)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   message "File is empty" VIEW-AS ALERT-BOX.
   RETURN.
END.
LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by customer number ".
       if order = 2 then put screen row 19 col 30 " Order by closing Date    ".
    END.

   IF must-add THEN DO:  /* ClosedCust -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
   END.

/* ADDING DISABLED
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:
           PROMPT-FOR ClosedCust.CustNum
           VALIDATE
              (ClosedCust.CustNum = "" OR
              NOT can-find(ClosedCust using  ClosedCust.CustNum),
              " " + string(INPUT ClosedCust.CustNum) +
              " already exists !").
           if input ClosedCust.CustNum = "" THEN LEAVE add-new.
           CREATE ClosedCust.
           ASSIGN
           ClosedCust.CustNum = INPUT FRAME lis ClosedCust.CustNum.
           UPDATE ClosedCust.
                  CustNum Date DateOpen State
                  .
           ASSIGN
           memory = recid(ClosedCust)
           xrecid = memory.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST ClosedCust
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE ClosedCust THEN LEAVE LOOP.
      NEXT LOOP.
   END.
*/

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND ClosedCust where recid(ClosedCust) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE ClosedCust THEN DO:
              FIND Customer where Customer.CustNum = ClosedCust.CustNum.
              IF AVAIL Customer THEN cust-name = Customer.CustName.
              else cust-name = "!! UNKNOWN !!".
              DISPLAY ClosedCust.CustNum cust-name ClosedCust.Date
                      ClosedCust.DateOpen ClosedCust.State ClosedCust.Printed 
                      ClosedCust.Called.
              rtab[FRAME-LINE] = recid(ClosedCust).
              IF order = 1 THEN FIND NEXT ClosedCust
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT ClosedCust USE-INDEX
              Date no-lock no-error.
       /*     ELSE IF order = 3 THEN FIND NEXT ClosedCust USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT ClosedCust USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
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
        ufk[1]= 35  ufk[2]= 28 ufk[3]= 911 ufk[4]= 912
        ufk[5]= 903 ufk[6]= 4  ufk[7]= 0   ufk[8]= 8   ufk[9]= 1
        ehto  = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW ClosedCust.CustNum ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) ClosedCust.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ClosedCust.Date ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) ClosedCust.Date WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        CHOOSE ROW ClosedCust.?? ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) ClosedCust.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW ClosedCust.??  ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) ClosedCust.? WITH FRAME sel.
      END.
*/
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
        FIND ClosedCust where recid(ClosedCust) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev ClosedCust
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev ClosedCust USE-INDEX
           Date no-lock no-error.
      /*   ELSE IF order = 3 THEN FIND prev ClosedCust USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev ClosedCust USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE ClosedCust THEN
              ASSIGN firstline = i memory = recid(ClosedCust).
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
           FIND ClosedCust where recid(ClosedCust) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev ClosedCust
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev ClosedCust USE-INDEX
           Date no-lock no-error.
      /*   ELSE IF order = 3 THEN FIND prev ClosedCust USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev ClosedCust USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE ClosedCust THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              FIND Customer where Customer.CustNum = ClosedCust.CustNum.
              IF AVAIL Customer THEN cust-name = Customer.CustName.
              else cust-name = "!! UNKNOWN !!".
              DISPLAY ClosedCust.CustNum cust-name ClosedCust.Date
                      ClosedCust.DateOpen ClosedCust.State ClosedCust.Printed 
                      ClosedCust.Called.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(ClosedCust)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND ClosedCust where recid(ClosedCust) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT ClosedCust
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT ClosedCust USE-INDEX
           Date no-lock no-error.
      /*   ELSE IF order = 3 THEN FIND NEXT ClosedCust USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT ClosedCust USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE ClosedCust THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              FIND Customer where Customer.CustNum = ClosedCust.CustNum.
              IF AVAIL Customer THEN cust-name = Customer.CustName.
              else cust-name = "!! UNKNOWN !!".
              DISPLAY ClosedCust.CustNum cust-name ClosedCust.Date
                      ClosedCust.DateOpen ClosedCust.State ClosedCust.Printed ClosedCust.Called.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(ClosedCust).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ClosedCust where recid(ClosedCust) = memory no-lock no-error.
        IF order = 1 THEN FIND prev ClosedCust
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev ClosedCust USE-INDEX
        Date no-lock no-error.
   /*   ELSE IF order = 3 THEN FIND prev ClosedCust USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev ClosedCust USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE ClosedCust THEN DO:
           memory = recid(ClosedCust).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev ClosedCust
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev ClosedCust USE-INDEX
              Date no-lock no-error.
       /*     ELSE IF order = 3 THEN FIND prev ClosedCust USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev ClosedCust USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE ClosedCust THEN memory = recid(ClosedCust).
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
           FIND ClosedCust where recid(ClosedCust) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       CustNum = ?.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE CustNum WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       IF CustNum <> ? THEN DO:
          FIND FIRST ClosedCust where ClosedCust.CustNum >= CustNum
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE ClosedCust THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  clcust/cust-nr was found */
          ASSIGN order = 1 memory = recid(ClosedCust) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       Date = ?.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE Date WITH FRAME haku-f2.
       HIDE FRAME haku-f2 no-pause.
       IF Date <> ? THEN DO:
          FIND FIRST ClosedCust where ClosedCust.Date >= Date
          USE-INDEX  Date no-lock no-error.
          IF NOT AVAILABLE ClosedCust THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  clcust/ was found */
          ASSIGN order = 2 memory = recid(ClosedCust) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     /* Change selected ROW IF State AND Printed are FALSE */
     else if lookup(nap,"3,f3") > 0 THEN DO:
        FIND ClosedCust where recid(ClosedCust) = rtab[FRAME-LINE].
        IF NOT ClosedCust.State AND NOT ClosedCust.Printed THEN DO:
           ASSIGN ok = FALSE.
           message "ARE YOU SURE YOU WANT TO CHANGE SELECTED ROW (Y/N) ?"
              UPDATE ok.
           IF ok THEN DO:
              DO TRANSAction:
                 IF ClosedCust.DateOpen = ? THEN ASSIGN ClosedCust.DateOpen = pvm.
                 ELSE                      ASSIGN ClosedCust.DateOpen = ?.
                 DISPLAY ClosedCust.DateOpen.
              END.
           END.
        END.
        ELSE IF ClosedCust.Printed = TRUE THEN DO:
           BELL.
           message "You can not change already Printed rows !".
           PAUSE 2 no-message.
        END.
        ELSE IF ClosedCust.State   = TRUE THEN DO:
           BELL.
           message "Opening is denied for this row !".
           PAUSE 2 no-message.
        END.
     END. /* f3 */

     /* Change ALL rows where State AND Printed are FALSE */
     else if lookup(nap,"4,f4") > 0 THEN DO:
        ASSIGN ok = FALSE.
        message "ARE YOU SURE YOU WANT TO CHANGE ALL (Y/N) ?" UPDATE ok.
        IF ok THEN DO:
           DO TRANSAction:
              FOR EACH ClosedCust where
                       ClosedCust.State   = FALSE AND
                       ClosedCust.Printed = FALSE.

                 IF ClosedCust.DateOpen = ? THEN ASSIGN ClosedCust.DateOpen = pvm.
                 ELSE                      ASSIGN ClosedCust.DateOpen = ?.

              END.
           END.
           ASSIGN must-print = TRUE.
           NEXT LOOP.
        END.
        HIDE MESSAGE no-pause.
     END. /* f4 */

     /* append numbers TO monthly File where State AND Printed are FALSE */
     if lookup(nap,"5,f5") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        whitelist = whitelist + 
                    "wlo-" + substr(string(Month,"999999"),3) + ".dat".
        PAUSE 0.

        FIND LAST ClosedCust where 
                  ClosedCust.DateOpen NE ?     AND
                  ClosedCust.Printed = FALSE
        no-lock no-error.

        IF AVAIL ClosedCust THEN DateOpen = ClosedCust.DateOpen.
        ELSE Date = ?.

        UPDATE DateOpen whitelist WITH FRAME wl.
        if whitelist = "" THEN DO:
           HIDE FRAME wl no-pause.
           NEXT LOOP.
        END.
        OUTPUT STREAM whitelist TO value(whitelist) append.

        FOR EACH ClosedCust exclusive-lock where
                 ClosedCust.Printed = FALSE AND
                 ClosedCust.State   = FALSE AND
                (IF DateOpen NE ? THEN
                    ClosedCust.DateOpen = DateOpen ELSE
                    ClosedCust.DateOpen NE ?),

            EACH CLI no-lock        where
                 CLI.CustNum = ClosedCust.CustNum

        BREAK
           BY ClosedCust.CustNum:

              PUT STREAM whitelist UNFORMATTED
                 substr(CLI.CLI,2)  SKIP.

              IF last-of(ClosedCust.CustNum) THEN DO:
                 ASSIGN ClosedCust.Printed = TRUE.
                 /* open customers in MthCall -file
                 that have been closed during the opening MONTH */
                 FIND FIRST MthCall where
                            MthCall.CustNum = ClosedCust.CustNum AND
                            MthCall.Month     = Month 
                 exclusive-lock no-error.
                 IF AVAIL MthCall THEN
                   ASSIGN MthCall.CloseDate  = ?
                          MthCall.CloseType = 0
                          MthCall.Printed = FALSE.

              END.

        END. /* FOR EACH */

        OUTPUT STREAM whitelist CLOSE.
        ASSIGN must-print = TRUE.
        HIDE FRAME wl no-pause.
        NEXT LOOP.

     END. /* f5 */

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND ClosedCust where recid(ClosedCust) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          ClosedCust.CustNum ClosedCust.Date ClosedCust.DateOpen ClosedCust.State
          ClosedCust.Printed ClosedCust.Called.

       IF order = 1 THEN FIND NEXT ClosedCust
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT ClosedCust USE-INDEX
       Date no-lock no-error.
  /*   ELSE IF order = 3 THEN FIND NEXT ClosedCust USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT ClosedCust USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE ClosedCust THEN memory = recid(ClosedCust).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND ClosedCust where recid(ClosedCust) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev ClosedCust
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev ClosedCust USE-INDEX
          Date no-lock no-error.
     /*   ELSE IF order = 3 THEN FIND prev ClosedCust USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev ClosedCust USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE ClosedCust THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(ClosedCust).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND ClosedCust where recid(ClosedCust) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          ClosedCust.CustNum ClosedCust.Date ClosedCust.DateOpen ClosedCust.State
          ClosedCust.Printed ClosedCust.Called.

       IF ok THEN DO:

           DELETE ClosedCust.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST ClosedCust
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
       FIND ClosedCust where recid(ClosedCust) = rtab[frame-line(sel)] exclusive-lock.
       IF ClosedCust.Printed = FALSE THEN DO:
          assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
          RUN Syst/ufkey.
          cfc = "lis". RUN Syst/ufcolor.
          DISPLAY ClosedCust.CustNum ClosedCust.Date ClosedCust.DateOpen.
          UPDATE ClosedCust.State.
          HIDE FRAME lis no-pause.
          DISPLAY ClosedCust.DateOpen ClosedCust.State
          WITH FRAME sel.
          xrecid = recid(ClosedCust).
       END.
       ELSE DO:
          BELL.
          message "You can not change already Printed rows !".
          PAUSE 2 no-message.
       END.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST ClosedCust
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST ClosedCust USE-INDEX
       Date no-lock no-error.
  /*  ELSE IF order = 3 THEN FIND FIRST ClosedCust USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST ClosedCust USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(ClosedCust) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST ClosedCust
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST ClosedCust USE-INDEX
       Date no-lock no-error.
  /*   ELSE IF order = 3 THEN FIND LAST ClosedCust USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST ClosedCust USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(ClosedCust) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

