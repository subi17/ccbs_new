/* -----------------------------------------------
  MODULE .......: NNXNYP
  FUNCTION .....: Naapuri-suuntanumeroiden yllAp
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 26-10-97
  changePVM ....: 18-05-99 jp - uright1 & uright2 added
                  11.09.02 jp - find name, find code renamed
                  04.03.03 tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'areapair'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF BUFFER xxrnom FOR AreaCode.
DEF VAR xx-name         LIKE xxrnom.AreaName            NO-UNDO.
DEF VAR AreaName         LIKE AreaCode.AreaName            NO-UNDO.
DEF VAR AreaCode          LIKE AreaPair.AreaCode  NO-UNDO.
DEF VAR NeigArea          LIKE AreaPair.NeigArea NO-UNDO.
DEF VAR xrecid          AS RECID                           init ?.
DEF VAR firstline       AS INT                    NO-UNDO  init 0.
DEF VAR order           AS INT                    NO-UNDO  init 1.
DEF VAR ordercount      AS INT                    NO-UNDO  init 2.
DEF VAR ufkey           AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline         AS INT                    NO-UNDO  init 0.
DEF VAR ex-order        AS INT                    NO-UNDO.
DEF VAR Memory          AS RECID                  NO-UNDO.
def var line            as int format "99"        NO-UNDO.
DEF VAR must-print      AS LOG                    NO-UNDO.
DEF VAR must-add        AS LOG                    NO-UNDO.
DEF VAR fr-header       AS CHAR                   NO-UNDO.
DEF VAR rtab            AS RECID EXTENT 24        NO-UNDO.
DEF VAR i               AS INT                    NO-UNDO.
def var ok              as log format "Yes/No"    NO-UNDO.

form
    AreaPair.AreaCode    /* column-label "A-ab.!riktnr"         */
    AreaName          /* column-label "A-Client's Reg/City"  */
    AreaPair.NeigArea     column-label "X-AREA"
    xx-name          /* column-label "AngrAnsande!ort/stad" */
WITH width 80 OVERLAY scroll 1 14 DOWN COLOR value(cfc)
    title color value(ctc) " " + ynimi + " Bordering areacode "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    AreaCode    label "Areacode ......"
    AreaCode.AreaName NO-LABEL SKIP
    NeigArea    label "Bordering Area "
    xx-name        NO-LABEL SKIP

WITH  OVERLAY ROW 4 centered
    COLOR value(cfc) TITLE COLOR value(ctc) fr-header WITH side-labels
    FRAME lis.

form /* angrAnsande riktnr. search WITH FIELD AreaCode */
    AreaCode
    help "Give A-client's areacode"
    with row 4 col 2 title color value(ctc) " FIND A-AB. AREACODE "
    COLOR value(cfc) side-labels OVERLAY FRAME f1.

form /* angrAnsande riktnr. search WITH FIELD NeigArea */
    NeigArea
    help "Give bordering region's areacode"
    with row 4 col 2 title color value(ctc) " FIND BORDERING AREA "
    COLOR value(cfc) side-labels OVERLAY FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST AreaPair
/* search condition */ no-lock no-error.
IF AVAILABLE AreaPair THEN ASSIGN
   Memory       = recid(AreaPair)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No area pairs available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print = FALSE
      must-add    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 33 "  By A-Client  ".
       if order = 2 then put screen row 19 col 33 "  By B-Client  ".
    END.

   IF must-add THEN DO:  /* AreaPair -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN ufkey.
         DO TRANSACTION:
            assign AreaCode = "" NeigArea = "".
            UPDATE
            AreaCode
            NeigArea
            WITH FRAME lis EDITING:
               READKEY. nap = keylabel(LASTKEY).
               IF lookup(nap,poisnap) > 0 THEN DO:
                  PAUSE 0.
                  if frame-field = "AreaCode" THEN DO:
                     ASSIGN FRAME lis AreaCode.
                     if AreaCode = "" THEN UNDO add-new, LEAVE add-new.
                     FIND AreaCode where AreaCode.AreaCode = AreaCode no-lock no-error.
                     IF NOT AVAIL AreaCode THEN DO:
                        BELL.
                        message "Unknown landnumber !".
                        NEXT.
                     END.
                     DISP AreaCode.AreaName WITH FRAME lis.
                  END.
                  else if frame-field = "NeigArea" THEN DO:
                     ASSIGN FRAME lis NeigArea.
                     if NeigArea = "" THEN DO:
                        NEXT-PROMPT AreaCode.
                        NEXT.
                     END.

                     IF NeigArea = AreaCode THEN DO:
                        BELL.  MESSAGE
                        "BAda numren Ar likadana !".
                        NEXT.
                     END.



                     FIND AreaCode where AreaCode.AreaCode = NeigArea no-lock no-error.
                     IF NOT AVAIL AreaCode THEN DO:
                        BELL.
                        message "Unknown landcode !".
                        NEXT.
                     END.
                     DISP AreaCode.AreaName @ xx-name WITH FRAME lis.

                     IF can-find(FIRST AreaPair where
                                       AreaPair.AreaCode = AreaCode AND
                                       AreaPair.NeigArea = NeigArea) THEN DO:
                        BELL.
                        message "Areanumber already exists !".
                        NEXT.
                     END.
                  END.
               END.
               APPLY LASTKEY.
            END. /* EDITING */
            CREATE AreaPair.
            ASSIGN
            AreaPair.AreaCode = AreaCode
            AreaPair.NeigArea = NeigArea.
            ASSIGN
            Memory = recid(AreaPair)
            xrecid = Memory.
         END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST AreaPair
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE AreaPair THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND AreaPair where recid(AreaPair) = Memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = Memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE AreaPair THEN DO:

               FIND AreaCode where AreaCode.AreaCode = AreaPair.AreaCode no-lock no-error.
               IF AVAIL AreaCode THEN AreaName = AreaCode.AreaName.
               else                 AreaName = "! UNKNOWN !".

               FIND xxrnom where xxrnom.AreaCode = AreaPair.NeigArea no-lock no-error.
               IF AVAIL xxrnom THEN xx-name = xxrnom.AreaName.
               else                 xx-name = "! UNKNOWN !".


               DISPLAY AreaPair.AreaCode AreaName AreaPair.NeigArea xx-name.

               rtab[FRAME-LINE] = recid(AreaPair).
               IF order = 1 THEN FIND NEXT AreaPair
               /* search condition */ no-lock no-error.
               ELSE IF order = 2 THEN FIND NEXT AreaPair USE-INDEX NeigArea
               /* search condition */ no-lock no-error.
         /*    ELSE IF order = 3 THEN FIND NEXT AreaPair USE-INDEX index-3
               /* search condition */ no-lock no-error.
               ELSE IF order = 4 THEN FIND NEXT AreaPair USE-INDEX index-4
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
         ufk[1]= 35  ufk[2]= 36 ufk[3]= 0 ufk[4]= 0
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         {uright1.i '"5,6"'}

         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW AreaPair.AreaCode ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaPair.AreaCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW AreaPair.NeigArea ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaPair.NeigArea WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
         CHOOSE ROW AreaPair.?? ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaPair.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
         CHOOSE ROW AreaPair.??  ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaPair.? WITH FRAME sel.
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
         ASSIGN firstline = 0 Memory = rtab[FRAME-LINE].
         FIND AreaPair where recid(AreaPair) = Memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev AreaPair
            /* search condition */ no-lock no-error.
            ELSE IF order = 2 THEN FIND prev AreaPair USE-INDEX NeigArea
            /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev AreaPair USE-INDEX index-3
            /* search condition */ no-lock no-error.
            ELSE IF order = 4 THEN FIND prev AreaPair USE-INDEX index-4
            /* search condition */ no-lock no-error.   */
            IF AVAILABLE AreaPair THEN
               ASSIGN firstline = i Memory = recid(AreaPair).
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
            FIND AreaPair where recid(AreaPair) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev AreaPair
            /* search condition */ no-lock no-error.
            ELSE IF order = 2 THEN FIND prev AreaPair USE-INDEX NeigArea
            /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev AreaPair USE-INDEX index-3
            /* search condition */ no-lock no-error.
            ELSE IF order = 4 THEN FIND prev AreaPair USE-INDEX index-4
            /* search condition */ no-lock no-error.   */
            IF NOT AVAILABLE AreaPair THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.

               FIND AreaCode where AreaCode.AreaCode = AreaPair.AreaCode no-lock no-error.
               IF AVAIL AreaCode THEN AreaName = AreaCode.AreaName.
               else                 AreaName = "! UNKNOWN !".

               FIND xxrnom where xxrnom.AreaCode = AreaPair.NeigArea no-lock no-error.
               IF AVAIL xxrnom THEN xx-name = xxrnom.AreaName.
               else                 xx-name = "! UNKNOWN !".

               DISPLAY AreaPair.AreaCode AreaName AreaPair.NeigArea xx-name.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(AreaPair)
               Memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND AreaPair where recid(AreaPair) = rtab[FRAME-DOWN] no-lock .
            IF order = 1 THEN FIND NEXT AreaPair
            /* search condition */ no-lock no-error.
            ELSE IF order = 2 THEN FIND NEXT AreaPair USE-INDEX NeigArea
            /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND NEXT AreaPair USE-INDEX index-3
            /* search condition */ no-lock no-error.
            ELSE IF order = 4 THEN FIND NEXT AreaPair USE-INDEX index-4
            /* search condition */ no-lock no-error.   */
            IF NOT AVAILABLE AreaPair THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.

               FIND AreaCode where AreaCode.AreaCode = AreaPair.AreaCode no-lock no-error.
               IF AVAIL AreaCode THEN AreaName = AreaCode.AreaName.
               else                 AreaName = "! UNKNOWN !".

               FIND xxrnom where xxrnom.AreaCode = AreaPair.NeigArea no-lock no-error.
               IF AVAIL xxrnom THEN xx-name = xxrnom.AreaName.
               else                 xx-name = "! UNKNOWN !".

               DISPLAY AreaPair.AreaCode AreaName AreaPair.NeigArea xx-name.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(AreaPair).
               /* finally LAST line's KeyValue is saved */
               Memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND AreaPair where recid(AreaPair) = Memory no-lock no-error.
         IF order = 1 THEN FIND prev AreaPair
         /* search condition */ no-lock no-error.
         ELSE IF order = 2 THEN FIND prev AreaPair USE-INDEX NeigArea
         /* search condition */ no-lock no-error.
   /*    ELSE IF order = 3 THEN FIND prev AreaPair USE-INDEX index-3
         /* search condition */ no-lock no-error.
         ELSE IF order = 4 THEN FIND prev AreaPair USE-INDEX index-4
         /* search condition */ no-lock no-error.   */
         IF AVAILABLE AreaPair THEN DO:
            Memory = recid(AreaPair).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev AreaPair
               /* search condition */ no-lock no-error.
               ELSE IF order = 2 THEN FIND prev AreaPair USE-INDEX NeigArea
               /* search condition */ no-lock no-error.
         /*    ELSE IF order = 3 THEN FIND prev AreaPair USE-INDEX index-3
               /* search condition */ no-lock no-error.
               ELSE IF order = 4 THEN FIND prev AreaPair USE-INDEX index-4
               /* search condition */ no-lock no-error.   */
               IF AVAILABLE AreaPair THEN Memory = recid(AreaPair).
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
            Memory = rtab[FRAME-DOWN].
            FIND AreaPair where recid(AreaPair) = Memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN ufcolor.
        AreaCode = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE AreaCode WITH FRAME f1.
        HIDE FRAME f1 no-pause.
        if AreaCode <> "" THEN DO:
           FIND FIRST AreaPair where AreaPair.AreaCode >= AreaCode
           /* search condition */ no-lock no-error.
           IF NOT AVAILABLE AreaPair THEN DO:
              BELL.
              message "NONE FOUND !".
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           /*  AreaPair/AreaCode was found */
           ASSIGN order = 1 Memory = recid(AreaPair) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN ufcolor.
        NeigArea = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE NeigArea WITH FRAME f2.
        HIDE FRAME f2 no-pause.
        if NeigArea <> "" THEN DO:
           FIND FIRST AreaPair where AreaPair.NeigArea >= NeigArea
           USE-INDEX NeigArea /* search condition */ no-lock no-error.
           IF NOT AVAILABLE AreaPair THEN DO:
              bell. message "NONE FOUND !".
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           /*  AreaPair/NeigArea was found */
           ASSIGN order = 2 Memory = recid(AreaPair) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

         must-add = TRUE.
         NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* removal */

        delline = FRAME-LINE.
        FIND AreaPair where recid(AreaPair) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc)
        AreaPair.AreaCode AreaPair.NeigArea AreaName xx-name.

        IF order = 1 THEN FIND NEXT AreaPair
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND NEXT AreaPair USE-INDEX NeigArea
        /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND NEXT AreaPair USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND NEXT AreaPair USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE AreaPair THEN Memory = recid(AreaPair).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND AreaPair where recid(AreaPair) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           IF order = 1 THEN FIND prev AreaPair
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev AreaPair USE-INDEX NeigArea
           /* search condition */ no-lock no-error.
     /*    ELSE IF order = 3 THEN FIND prev AreaPair USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev AreaPair USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE AreaPair THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              Memory = recid(AreaPair).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND AreaPair where recid(AreaPair) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
        COLOR DISPLAY value(ccc)
        AreaPair.AreaCode AreaPair.NeigArea AreaName xx-name.
        IF ok THEN DO:

            DELETE AreaPair.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST AreaPair
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


     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST AreaPair
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND FIRST AreaPair USE-INDEX NeigArea
        /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND FIRST AreaPair USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND FIRST AreaPair USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        ASSIGN Memory = recid(AreaPair) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST AreaPair
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND LAST AreaPair USE-INDEX NeigArea
        /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND LAST AreaPair USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND LAST AreaPair USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        ASSIGN Memory = recid(AreaPair) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

