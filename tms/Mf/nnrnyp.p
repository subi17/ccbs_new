/* ---------------------------------------------------------------
  MODULE .......: NNRNYP.P
  FUNCTION .....: UnderhAll av svenska riktnummeromrAde
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 14-07-97
  changePVM ....: 03-09-97 pt
                  26.10.97 pt f4: RUN Mf/nnaryp.p
                  15.01.98 pt NEW FIELD rn-ls
                  17.05.99 jp uright1 & uright2 added  
                  11.09.02 jp hide message
                  30.10.02 jr Eventlog
                  04.03.03 tk tokens
  Version ......: M15
  ------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'areacode'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR haku-st-nr   LIKE AreaCode.TrafficArea         NO-UNDO.
DEF VAR haku-rn-rnr  LIKE AreaCode.AreaCode        NO-UNDO.
DEF VAR haku-AreaName LIKE AreaCode.AreaName       NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR firstline    AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR ordercount   AS INT                    NO-UNDO  init 3.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline      AS INT                    NO-UNDO  init 0.
DEF VAR ex-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
def var line         as int format "99"        NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR fr-header    AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"    NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhAreaCode AS HANDLE NO-UNDO.
   lhAreaCode = BUFFER AreaCode:HANDLE.
   RUN StarEventInitialize(lhAreaCode).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhAreaCode).
   END.
END.


form
    AreaCode.TrafficArea
    AreaPlan.AreaName              format "x(13)"
    AreaCode.AreaCode     /* COLUMN-LABEL FORMAT */
    AreaCode.AreaName
    AreaCode.POI
    AreaCode.Local
WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi + " National Areacodes " +
     string(pvm,"99-99-99") + " " FRAME sel.

form
    AreaCode.TrafficArea   label "Traffic Area"  
    AreaPlan.AreaName        label "Name of TA"
    AreaCode.AreaCode  label "Area Code"
    AreaCode.AreaName label "Name of Area"
    AreaCode.POI
    AreaCode.Local

    /* sd */        /* LABEL FORMAT */
    /* ld */
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 1 columns
    FRAME lis.

form /* SamtrafiksomrAde search WITH FIELD TrafficArea */
    haku-st-nr
    help "Give area "
    with row 4 col 2 title color value(ctc) " FIND AREA "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

form /* SamtrafiksomrAde search WITH FIELD AreaCode */
    haku-rn-rnr
    help "Give a area code or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND AREACODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f2.

form /* SamtrafiksomrAde search WITH FIELD AreaCode */
    haku-AreaName
    help "Give a area Name or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND AREANAME "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f3.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST AreaCode
/* search condition */ no-lock USE-INDEX AreaCode no-error.
IF AVAILABLE AreaCode THEN ASSIGN
   Memory       = recid(AreaCode)
   must-print = TRUE
   must-add    = FALSE
   order = 2.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No Areacodes available !" VIEW-AS ALERT-BOX.
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
       if order = 1 then put screen row 19 col 32 "    By Number    ".
       if order = 2 then put screen row 19 col 32 "   By AreaCode   ".
       if order = 3 then put screen row 19 col 32 "  By RegionName  ".
    END.

   IF must-add THEN DO:  /* AreaCode -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN Syst/ufkey.p.
         DO TRANSACTION:
            PROMPT-FOR AreaCode.TrafficArea
            VALIDATE
               (AreaCode.TrafficArea = 0 OR  can-find(FIRST AreaPlan where
               AreaPlan.TrafficArea = INPUT FRAME lis AreaCode.TrafficArea),
               "Unknown traffic area !").
            IF INPUT AreaCode.TrafficArea = 0 THEN LEAVE add-new.
            CREATE AreaCode.
            ASSIGN AreaCode.TrafficArea.

            FIND AreaPlan of AreaCode no-lock.
            DISP AreaPlan.AreaName WITH FRAME lis.

            UPDATE AreaCode.AreaCode
                   AreaCode.AreaName
                   AreaCode.POI
                   AreaCode.Local
            WITH FRAME lis EDITING:
               READKEY.
                  IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
                     IF FRAME-FIELD = "AreaCode" THEN DO:
                        HIDE MESSAGE NO-PAUSE.
                        IF CAN-FIND(AreaCode WHERE AreaCode.AreaCode =
                                    INPUT AreaCode.AreaCode)
                        THEN DO:
                           BELL.
                           MESSAGE "This Areacode already exists !".
                           NEXT.         
                        END.            
                     END.
                  END.
               APPLY LASTKEY.
            END.
            IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhAreaCode).   
            ASSIGN
            Memory = recid(AreaCode)
            xrecid = Memory.
         END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST AreaCode
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE AreaCode THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND AreaCode where recid(AreaCode) = Memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = Memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.
         repeat WITH FRAME sel:
            IF AVAILABLE AreaCode THEN DO:
               FIND AreaPlan of AreaCode no-lock no-error.
               DISPLAY AreaCode.TrafficArea AreaCode.AreaCode
                   AreaPlan.AreaName AreaCode.POI AreaCode.POI AreaCode.Local
                  AreaCode.AreaName /* sd */.
               rtab[FRAME-LINE] = recid(AreaCode).
               IF order = 1 THEN FIND NEXT AreaCode
               /* search condition */ no-lock no-error.
               ELSE IF order = 2 THEN FIND NEXT AreaCode USE-INDEX AreaCode
               /* search condition */ no-lock no-error.
               ELSE IF order = 3 THEN FIND NEXT AreaCode USE-INDEX AreaName
               /* search condition */ no-lock no-error.
            /* ELSE IF order = 4 THEN FIND NEXT AreaCode USE-INDEX index-4
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
         ufk[1]= 133 ufk[2]= 889 ufk[3]= 717 ufk[4]= 888
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW AreaCode.TrafficArea {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaCode.TrafficArea WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW AreaCode.AreaCode {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaCode.AreaCode WITH FRAME sel.
      END.
      IF order = 3 THEN DO:
         CHOOSE ROW AreaCode.AreaName {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaCode.AreaName WITH FRAME sel.
      END.
   /* ELSE IF order = 4 THEN DO:
         CHOOSE ROW AreaCode.??  {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaCode.? WITH FRAME sel.
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
         FIND AreaCode where recid(AreaCode) = Memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev AreaCode
            /* search condition */ no-lock no-error.
            ELSE IF order = 2 THEN FIND prev AreaCode USE-INDEX AreaCode
            /* search condition */ no-lock no-error.
            ELSE IF order = 3 THEN FIND prev AreaCode USE-INDEX AreaName
            /* search condition */ no-lock no-error.
         /* ELSE IF order = 4 THEN FIND prev AreaCode USE-INDEX index-4
            /* search condition */ no-lock no-error.   */
            IF AVAILABLE AreaCode THEN
               ASSIGN firstline = i Memory = recid(AreaCode).
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
            FIND AreaCode where recid(AreaCode) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev AreaCode
            /* search condition */ no-lock no-error.
            ELSE IF order = 2 THEN FIND prev AreaCode USE-INDEX AreaCode
            /* search condition */ no-lock no-error.
            ELSE IF order = 3 THEN FIND prev AreaCode USE-INDEX AreaName
            /* search condition */ no-lock no-error.
         /* ELSE IF order = 4 THEN FIND prev AreaCode USE-INDEX index-4
            /* search condition */ no-lock no-error.   */
            IF NOT AVAILABLE AreaCode THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               FIND AreaPlan of AreaCode no-lock no-error.
               DISPLAY AreaCode.TrafficArea AreaCode.AreaCode
                   AreaPlan.AreaName AreaCode.POI AreaCode.POI AreaCode.Local
                       AreaCode.AreaName /* sd */.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(AreaCode)
               Memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND AreaCode where recid(AreaCode) = rtab[FRAME-DOWN] no-lock .
            IF order = 1 THEN FIND NEXT AreaCode
            /* search condition */ no-lock no-error.
            ELSE IF order = 2 THEN FIND NEXT AreaCode USE-INDEX AreaCode
            /* search condition */ no-lock no-error.
            ELSE IF order = 3 THEN FIND NEXT AreaCode USE-INDEX AreaName
            /* search condition */ no-lock no-error.
         /* ELSE IF order = 4 THEN FIND NEXT AreaCode USE-INDEX index-4
            /* search condition */ no-lock no-error.   */
            IF NOT AVAILABLE AreaCode THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               FIND AreaPlan of AreaCode no-lock no-error.
               DISPLAY AreaCode.TrafficArea AreaCode.AreaCode
                   AreaPlan.AreaName AreaCode.POI AreaCode.POI AreaCode.Local
                       AreaCode.AreaName /* sd */.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(AreaCode).
               /* finally LAST line's KeyValue is saved */
               Memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
         Memory = rtab[1].
         FIND AreaCode where recid(AreaCode) = Memory no-lock no-error.
         IF order = 1 THEN FIND prev AreaCode
         /* search condition */ no-lock no-error.
         ELSE IF order = 2 THEN FIND prev AreaCode USE-INDEX AreaCode
         /* search condition */ no-lock no-error.
         ELSE IF order = 3 THEN FIND prev AreaCode USE-INDEX AreaName
         /* search condition */ no-lock no-error.
      /* ELSE IF order = 4 THEN FIND prev AreaCode USE-INDEX index-4
         /* search condition */ no-lock no-error.   */
         IF AVAILABLE AreaCode THEN DO:
            Memory = recid(AreaCode).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev AreaCode
               /* search condition */ no-lock no-error.
               ELSE IF order = 2 THEN FIND prev AreaCode USE-INDEX AreaCode
               /* search condition */ no-lock no-error.
               ELSE IF order = 3 THEN FIND prev AreaCode USE-INDEX AreaName
               /* search condition */ no-lock no-error.
            /* ELSE IF order = 4 THEN FIND prev AreaCode USE-INDEX index-4
               /* search condition */ no-lock no-error.   */
               IF AVAILABLE AreaCode THEN Memory = recid(AreaCode).
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
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL. PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            Memory = rtab[FRAME-DOWN].
            FIND AreaCode where recid(AreaCode) = Memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN Syst/ufcolor.p.
        haku-st-nr = 0.
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE haku-st-nr WITH FRAME haku-f1.
        HIDE FRAME haku-f1 no-pause.
        IF haku-st-nr <> 0 THEN DO:
           FIND FIRST AreaCode where AreaCode.TrafficArea >= haku-st-nr
           /* search condition */ no-lock no-error.
           IF NOT AVAILABLE AreaCode THEN DO:
              BELL.
              message "NONE FOUND !".
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           /*  AreaCode/st-nr was found */
           ASSIGN order = 1 Memory = recid(AreaCode) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.p.
        haku-rn-rnr = "".
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE haku-rn-rnr WITH FRAME haku-f2.
        HIDE FRAME haku-f2 no-pause.
        if haku-rn-rnr <> "" THEN DO:
           FIND FIRST AreaCode where AreaCode.AreaCode >= haku-rn-rnr
           USE-INDEX AreaCode /* search condition */ no-lock no-error.
           IF NOT AVAILABLE AreaCode THEN DO:
              bell. message "NONE FOUND !".
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           /*  AreaCode/rn-rnr was found */
           ASSIGN order = 2 Memory = recid(AreaCode) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */
     /* Haku sarakk. 2 */

     else if lookup(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.p.
        haku-AreaName = "".
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE haku-AreaName WITH FRAME haku-f3.
        HIDE FRAME haku-f3 no-pause.
        if haku-AreaName <> "" THEN DO:
           FIND FIRST AreaCode where AreaCode.AreaName >= haku-AreaName
           USE-INDEX AreaName /* search condition */ no-lock no-error.
           IF NOT AVAILABLE AreaCode THEN DO:
              bell. message "NONE FOUND !".
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           /*  AreaCode/AreaName was found */
           ASSIGN order = 3 Memory = recid(AreaCode) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 3 */

     else if lookup(nap,"4,f4") > 0 THEN DO TRANSACTION:  /* Angr. rnr */
        delline = FRAME-LINE.
        FIND AreaCode where recid(AreaCode) = rtab[FRAME-LINE] no-lock.
        RUN Mf/nnaryp.p (AreaCode.AreaCode).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

         must-add = TRUE.
         NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* removal */

        delline = FRAME-LINE.
        FIND AreaCode where recid(AreaCode) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc)
        AreaCode.TrafficArea AreaCode.AreaCode AreaPlan.AreaName AreaCode.POI AreaCode.Local
        AreaCode.AreaName /* sd */.

        IF order = 1 THEN FIND NEXT AreaCode
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND NEXT AreaCode USE-INDEX AreaCode
        /* search condition */ no-lock no-error.
        ELSE IF order = 3 THEN FIND NEXT AreaCode USE-INDEX AreaName
        /* search condition */ no-lock no-error.
     /* ELSE IF order = 4 THEN FIND NEXT AreaCode USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE AreaCode THEN Memory = recid(AreaCode).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND AreaCode where recid(AreaCode) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           IF order = 1 THEN FIND prev AreaCode
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev AreaCode USE-INDEX AreaCode
           /* search condition */ no-lock no-error.
           ELSE IF order = 3 THEN FIND prev AreaCode USE-INDEX AreaName
           /* search condition */ no-lock no-error.
        /* ELSE IF order = 4 THEN FIND prev AreaCode USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE AreaCode THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              Memory = recid(AreaCode).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND AreaCode where recid(AreaCode) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
        COLOR DISPLAY value(ccc)
        AreaCode.TrafficArea AreaCode.AreaCode AreaPlan.AreaName AreaCode.POI AreaCode.Local
        AreaCode.AreaName /* sd */.
        IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhAreaCode).
            DELETE AreaCode.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST AreaCode
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
     DO WITH FRAME lis TRANSACTION:
        /* change */

        FIND AreaCode where recid(AreaCode) = rtab[frame-line(sel)]
        exclusive-lock.
        assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
        RUN Syst/ufkey.p.
        cfc = "lis". RUN Syst/ufcolor.p.
        FIND AreaPlan of AreaCode no-lock no-error.
        DISPLAY 
           AreaCode.TrafficArea 
           AreaPlan.AreaName 
           AreaCode.AreaCode
           AreaCode.POI
           AreaCode.AreaName
           AreaCode.local
        WITH FRAME lis.   

        IF lcRight = "RW" THEN DO:

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhAreaCode).
           UPDATE 
              /* AreaCode.TrafficArea */ 
              AreaCode.AreaName 
              AreaCode.POI 
              AreaCode.Local
              /* ld */
           WITH FRAME lis.

           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhAreaCode).       

        END.
        ELSE PAUSE.

        HIDE FRAME lis no-pause.

        DISPLAY 
           AreaCode.AreaCode 
           AreaPlan.AreaName 
           AreaCode.POI 
           AreaCode.AreaName 
           /* sd */
           AreaCode.Local
        WITH FRAME sel.
        xrecid = recid(AreaCode).
     END.

     else if lookup(nap,"home") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST AreaCode
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND FIRST AreaCode USE-INDEX AreaCode
        /* search condition */ no-lock no-error.
        ELSE IF order = 3 THEN FIND FIRST AreaCode USE-INDEX AreaName
        /* search condition */ no-lock no-error.
     /* ELSE IF order = 4 THEN FIND FIRST AreaCode USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        ASSIGN Memory = recid(AreaCode) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST AreaCode
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND LAST AreaCode USE-INDEX AreaCode
        /* search condition */ no-lock no-error.
        ELSE IF order = 3 THEN FIND LAST AreaCode USE-INDEX AreaName
        /* search condition */ no-lock no-error.
     /* ELSE IF order = 4 THEN FIND LAST AreaCode USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        ASSIGN Memory = recid(AreaCode) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

