/* -----------------------------------------------
  MODULE .......: NNPYYP
  FUNCTION .....: PyhApAivien yllApito
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 28-04-96
  MODIFIED .....: 25-06-98 vast => ok
                  18-05-99 jp   => uright1 & uright2 added  
                  26.04.02 tk - eventlogging added
                  05.03.03 tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'natholiday'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhNatHoliday AS HANDLE NO-UNDO.
   lhNatHoliday = BUFFER NatHoliday:HANDLE.
   RUN StarEventInitialize(lhNatHoliday).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhNatHoliday).
   END.

END.

DEF shared VAR siirto AS CHAR.

def var ok          as lo format "Yes/No"   NO-UNDO.
DEF VAR haku        LIKE NatHoliday.Holiday      NO-UNDO.
DEF VAR haku2       LIKE NatHoliday.HName   NO-UNDO.
DEF VAR firstline   AS INT                  NO-UNDO.
DEF VAR order       AS INT                  NO-UNDO.
DEF VAR ex-order    AS INT                  NO-UNDO.
DEF VAR memory      AS RECID                NO-UNDO.
def var line        as int format "99"      NO-UNDO.
DEF VAR delline     AS INT                  NO-UNDO.
DEF VAR must-print  AS LOG                  NO-UNDO.
DEF VAR must-add    AS LOG                  NO-UNDO.
DEF VAR ufkey       AS LOG                  NO-UNDO.
DEF VAR fr-header   AS CHAR.
DEF VAR rtab        AS RECID EXTENT 24      NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR xrecid      AS RECID.

form
    NatHoliday.Holiday      /* column-label "Date"        */
    NatHoliday.HName    column-label "Explanation" 

    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " MID-WEEK Holiday "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    NatHoliday.Holiday      /* label "Date"               */
    NatHoliday.HName   /* label "Explanation"        */


    WITH  OVERLAY ROW 8 col 5
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 1 columns
    FRAME lis.

form /* ArkipyhA :n tunnuksella hakua varten */
    haku
    help "Give holiday's code or beginning of it"
    with row 4 col 2 title color value(ctc) " HOLIDAYCODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* ArkipyhA :n nimella hakua varten */
    haku2
    help "Give holiday's Name or beginning of it"
    with row 4 col 2 title color value(ctc) " HOLIDAYNAME "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.
   FIND FIRST NatHoliday no-lock no-error.
IF AVAILABLE NatHoliday THEN ASSIGN memory = recid(NatHoliday)
   must-print = TRUE must-add    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No national holidays available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN memory = ? must-print = FALSE must-add    = TRUE.
END.

ASSIGN xrecid = ? delline = 0 ufkey = TRUE order = 1 firstline = 0.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 34 "   By Date   ".
       if order = 2 then put screen row 19 col 34 "   By Name   ".
    END.

   IF must-add THEN DO:  /* NatHoliday -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN ufkey.
         DO TRANSAction:
            PROMPT-FOR NatHoliday.Holiday
            VALIDATE
               (NatHoliday.Holiday = ? OR
               NOT can-find(NatHoliday using  NatHoliday.Holiday),
               "Holiday:A " + string(INPUT NatHoliday.Holiday) +
               " already exists !").
            IF INPUT NatHoliday.Holiday = ? THEN LEAVE add-new.
            CREATE NatHoliday.
            ASSIGN
            NatHoliday.Holiday = INPUT FRAME lis NatHoliday.Holiday.
            UPDATE NatHoliday.HName

                   .
            ASSIGN
            memory = recid(NatHoliday)
            xrecid = memory.
         END.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhNatHoliday).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST NatHoliday no-lock no-error.
      IF NOT AVAILABLE NatHoliday THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND NatHoliday where recid(NatHoliday) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE NatHoliday THEN DO:
               DISPLAY NatHoliday.Holiday NatHoliday.HName
                       .
               rtab[FRAME-LINE] = recid(NatHoliday).
               IF order = 1 THEN FIND NEXT NatHoliday no-lock no-error.
               ELSE IF order = 2 THEN FIND NEXT NatHoliday USE-INDEX HName
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
         ufk[1]= 28  ufk[2]= 717 ufk[3]= 0 ufk[4]= 0
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW NatHoliday.Holiday ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) NatHoliday.Holiday WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW NatHoliday.HName ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) NatHoliday.HName WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 3 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 2.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND NatHoliday where recid(NatHoliday) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev NatHoliday no-lock no-error.
            ELSE IF order = 2 THEN FIND prev NatHoliday USE-INDEX HName
            no-lock no-error.
            IF AVAILABLE NatHoliday THEN                     
               ASSIGN firstline = i memory = recid(NatHoliday).
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
            FIND NatHoliday where recid(NatHoliday) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev NatHoliday no-lock no-error.
            ELSE IF order = 2 THEN FIND prev NatHoliday USE-INDEX HName
            no-lock no-error.
            IF NOT AVAILABLE NatHoliday THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY NatHoliday.Holiday NatHoliday.HName
                       .
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(NatHoliday)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND NatHoliday where recid(NatHoliday) = rtab[FRAME-DOWN] no-lock .
            IF order = 1 THEN FIND NEXT NatHoliday no-lock no-error.
            ELSE IF order = 2 THEN FIND NEXT NatHoliday USE-INDEX HName
            no-lock no-error.
            IF NOT AVAILABLE NatHoliday THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               DISPLAY NatHoliday.Holiday NatHoliday.HName
                       .
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(NatHoliday).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND NatHoliday where recid(NatHoliday) = memory no-lock no-error.
         IF order = 1 THEN FIND prev NatHoliday no-lock no-error.
         ELSE IF order = 2 THEN FIND prev NatHoliday USE-INDEX HName
         no-lock no-error.
         IF AVAILABLE NatHoliday THEN DO:
            memory = recid(NatHoliday).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev NatHoliday no-lock no-error.
               ELSE IF order = 2 THEN FIND prev NatHoliday USE-INDEX HName
               no-lock no-error.
               IF AVAILABLE NatHoliday THEN memory = recid(NatHoliday).
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
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND NatHoliday where recid(NatHoliday) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
        cfc = "puyr". RUN ufcolor.
        haku = ?.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE haku WITH FRAME hayr.
        HIDE FRAME hayr no-pause.
        IF haku <> ? THEN DO:
           FIND FIRST NatHoliday where NatHoliday.Holiday = INPUT haku
           no-lock no-error.
           IF NOT AVAILABLE NatHoliday THEN DO:
              FIND FIRST NatHoliday where NatHoliday.Holiday ge INPUT haku
              no-lock no-error.
              IF NOT AVAILABLE NatHoliday THEN DO:
                 FIND FIRST NatHoliday where NatHoliday.Holiday le INPUT haku
                 no-lock no-error.
                 IF NOT AVAILABLE NatHoliday THEN DO:
                    BELL.
                    message "CAN'T FIND".
                    PAUSE 1 no-message.
                    NEXT BROWSE.
                 END.
              END.
           END.
           /*  ArkipyhA  was found */
           ASSIGN
           order = 1 memory = recid(NatHoliday) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
        cfc = "puyr". RUN ufcolor.
        haku2 = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE haku2 WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.
        if haku2 <> "" THEN DO:
           FIND FIRST NatHoliday where NatHoliday.HName = INPUT haku2
           no-lock no-error.
           IF NOT AVAILABLE NatHoliday THEN DO:
              FIND FIRST NatHoliday where NatHoliday.HName ge INPUT haku2
              no-lock no-error.
              IF NOT AVAILABLE NatHoliday THEN DO:
                 FIND FIRST NatHoliday where NatHoliday.HName le INPUT haku2
                 no-lock no-error.
                 IF NOT AVAILABLE NatHoliday THEN DO:
                    BELL.
                    message "CAN'T FIND".
                    PAUSE 1 no-message.
                    NEXT BROWSE.
                 END.
              END.
           END.
           /*  ArkipyhA  was found */
           ASSIGN
             order = 2
             memory = recid(NatHoliday)
             must-print = TRUE.
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
        FIND NatHoliday where recid(NatHoliday) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) NatHoliday.Holiday NatHoliday.HName
        .

        IF order = 1 THEN FIND NEXT NatHoliday no-lock no-error.
        ELSE IF order = 2 THEN FIND NEXT NatHoliday USE-INDEX HName
        no-lock no-error.
        IF AVAILABLE NatHoliday THEN memory = recid(NatHoliday).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND NatHoliday where recid(NatHoliday) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           IF order = 1 THEN FIND prev NatHoliday no-lock no-error.
           ELSE IF order = 2 THEN FIND prev NatHoliday USE-INDEX HName
           no-lock no-error.
           IF AVAILABLE NatHoliday THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(NatHoliday).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND NatHoliday where recid(NatHoliday) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        message " ARE YOU SURE YOU WANT TO REMOVE (Y/N) " UPDATE ok.
        COLOR DISPLAY value(ccc) NatHoliday.Holiday NatHoliday.HName
        .
        IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhNatHoliday).

            DELETE NatHoliday.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST NatHoliday) THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     DO WITH FRAME lis TRANSAction:
        /* change */
        {Syst/uright2.i}
        FIND NatHoliday where recid(NatHoliday) = rtab[frame-line(sel)]
        exclusive-lock.
        assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
        RUN ufkey.
        cfc = "lis". RUN ufcolor.
        DISPLAY NatHoliday.Holiday.

        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhNatHoliday).

        UPDATE NatHoliday.HName

               .
        HIDE FRAME lis no-pause.
        DISPLAY NatHoliday.HName

        WITH FRAME sel.
        xrecid = recid(NatHoliday).

        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhNatHoliday).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST NatHoliday no-lock no-error.
        ELSE IF order = 2 THEN FIND FIRST NatHoliday USE-INDEX HName
        no-lock no-error.
        ASSIGN memory = recid(NatHoliday) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST NatHoliday no-lock no-error.
        ELSE IF order = 2 THEN FIND LAST NatHoliday USE-INDEX HName
        no-lock no-error.
        ASSIGN memory = recid(NatHoliday) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

