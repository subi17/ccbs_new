/* -----------------------------------------------
  MODULE .......: DIRMARKSE.P
  FUNCTION .....: DMarketing BROWSE / HELP
  APPLICATION ..: 
  AUTHOR .......: ht
  CREATED ......: 19.03.02
  CHANGE  ......: 27.03.03 tk don't use frame alku
                  16.09.03 jp Brand 
  ------------------------------------------------------ */

{Syst/commali.i}

DEF SHARED VAR siirto AS CHAR.

DEF VAR haku        LIKE DMarketing.DirMark     NO-UNDO.
DEF VAR haku2       LIKE DMarketing.DirMarkName NO-UNDO.
DEF VAR firstline   AS INT                  NO-UNDO.
DEF VAR order       AS INT                  NO-UNDO init 1.
DEF VAR ex-order    AS INT                  NO-UNDO.
DEF VAR memory      AS RECID                NO-UNDO.
DEF VAR line        AS INT FORMAT "99"      NO-UNDO.
DEF VAR delline     AS INT                  NO-UNDO.
DEF VAR must-print  AS LOG                  NO-UNDO.
DEF VAR ufkey       AS LOG                  NO-UNDO.

DEF VAR fr-header   AS CHAR.
DEF VAR tuhaku      AS CHAR FORMAT "x(30)"  NO-UNDO.
DEF VAR nrohaku     AS LOG                  NO-UNDO.
DEF VAR haettava    AS LOG INIT TRUE        NO-UNDO.
DEF VAR numero      AS LOG                  NO-UNDO.
DEF VAR rtab        AS RECID EXTENT 24      NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR xrecid      AS RECID.

FORM                              
    DMarketing.DirMarkName  FORMAT "x(21)" /* column-label "Form's name" */
    DMarketing.DirMark                 /* column-label "Form's code" */

    WITH CENTERED OVERLAY SCROLL 1 13 DOWN ROW 3
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " Direct Marketing " FRAME sel.
cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
   FIND FIRST DMarketing USE-INDEX DirMarkName WHERE 
              DMArketing.Brand = gcBrand 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DMarketing THEN DO:
      BELL.
      MESSAGE "There isn't any direct marketing codes - hit ENTER !".
      PAUSE NO-MESSAGE.
      RETURN.
   END.

   ASSIGN
   haettava  = TRUE 
   xrecid    = ? 
   delline   = 0 
   ufkey     = TRUE 
   firstline = 0 
   siirto    = ?
   memory    = recid(DMarketing)
   must-print = true. 

view frame sel.

LOOP:
REPEAT WITH FRAME sel:

    IF haettava THEN DO:

       ASSIGN haettava = FALSE nrohaku = FALSE.
       PAUSE 0 NO-MESSAGE.
    END.

    IF order <> ex-order THEN DO:
       ex-order = order.
       IF order = 2 THEN PUT SCREEN ROW 19 COL 36 " By Code ".
       IF order = 1 THEN PUT SCREEN ROW 19 COL 36 " By Name ".
    END.

print-line:
   DO :
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND DMarketing WHERE RECID(DMarketing) = memory NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose keyvalue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         REPEAT WITH FRAME sel:
            IF AVAILABLE DMarketing THEN DO:
               DISPLAY DMarketing.DirMark DMarketing.DirMarkName.
               rtab[FRAME-LINE] = RECID(DMarketing).
               IF order = 2 THEN FIND NEXT DMarketing
               USE-INDEX DirMark WHERE DMarketing.Brand = gcBrand 
               NO-LOCK NO-ERROR.
               ELSE IF order = 1 THEN FIND NEXT DMarketing
               USE-INDEX DirMarkName  WHERE DMarketing.Brand = gcBrand 
               NO-LOCK NO-ERROR.
            END.
            ELSE DO:
               CLEAR NO-PAUSE.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         UP FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline = 0
                must-print = FALSE.
         PAUSE 0 NO-MESSAGE.

         /* one page of data has been printed AND
         the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 0  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 11 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3   ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 2 THEN DO:
         CHOOSE ROW DMarketing.DirMark {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) DMarketing.DirMark WITH FRAME sel.
      END.
      ELSE IF order = 1 THEN DO:
        CHOOSE ROW DMarketing.DirMarkName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) DMarketing.DirMarkName WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = KEYLABEL(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 3 THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 2.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND DMarketing WHERE RECID(DMarketing) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 2 THEN FIND PREV DMarketing
            USE-INDEX DirMark  WHERE DMarketing.Brand = gcBrand 
            NO-LOCK NO-ERROR.
            ELSE IF order = 1 THEN FIND PREV DMarketing
            USE-INDEX DirMarkName  WHERE DMarketing.Brand = gcBrand 
            NO-LOCK NO-ERROR.
            IF AVAILABLE DMarketing THEN
               ASSIGN firstline = i memory = RECID(DMarketing).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? THEN DO:
         BELL.
         MESSAGE "You are on a empty ROW, move upwards !".
         PAUSE 1 NO-MESSAGE.
         NEXT.
      END.

      ASSIGN nap = KEYLABEL(LASTKEY).

      /* previous line */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND DMarketing WHERE RECID(DMarketing) = rtab[1] NO-LOCK.
            IF order = 2 THEN FIND PREV DMarketing
            USE-INDEX DirMark  WHERE DMarketing.Brand = gcBrand 
            NO-LOCK NO-ERROR.
            ELSE IF order = 1 THEN FIND PREV DMarketing WHERE 
                                             DMarketing.Brand = gcBrand 
             USE-INDEX DirMarkName NO-LOCK NO-ERROR.
            IF NOT AVAILABLE DMarketing THEN DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               SCROLL DOWN.
               DISPLAY DMarketing.DirMark DMarketing.DirMarkName.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = RECID(DMarketing)
               memory = rtab[1].
            END.
         END.
         ELSE UP 1.
      END. /* previous line */

      /* NEXT line */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND DMarketing WHERE RECID(DMarketing) = rtab[FRAME-DOWN] NO-LOCK .
            IF order = 2 THEN FIND NEXT DMarketing
            USE-INDEX DirMark  WHERE DMarketing.Brand = gcBrand
            NO-LOCK NO-ERROR.
            ELSE IF order = 1 THEN FIND NEXT DMarketing
            USE-INDEX DirMarkName  WHERE DMarketing.Brand = gcBrand
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE DMarketing THEN DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               SCROLL UP.
               DISPLAY DMarketing.DirMark DMarketing.DirMarkName.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = RECID(DMarketing).
               /* finally LAST line's keyvalue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND DMarketing WHERE RECID(DMarketing) = memory NO-LOCK NO-ERROR.
         IF order = 2 THEN FIND PREV DMarketing
         USE-INDEX DirMark  WHERE DMarketing.Brand = gcBrand 
         NO-LOCK NO-ERROR.
         ELSE IF order = 1 THEN FIND PREV DMarketing
         USE-INDEX DirMarkName  WHERE DMarketing.Brand = gcBrand 
         NO-LOCK NO-ERROR.
         IF AVAILABLE DMarketing THEN DO:
            memory = RECID(DMarketing).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 2 THEN FIND PREV DMarketing
               USE-INDEX DirMark  WHERE DMarketing.Brand = gcBrand 
               NO-LOCK NO-ERROR.
               ELSE IF order = 1 THEN FIND PREV DMarketing
               USE-INDEX DirMarkName  WHERE DMarketing.Brand = gcBrand 
               NO-LOCK NO-ERROR.
               IF AVAILABLE DMarketing THEN memory = RECID(DMarketing).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            MESSAGE "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 NO-MESSAGE.
         END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            MESSAGE "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 NO-MESSAGE.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND DMarketing WHERE RECID(DMarketing) = memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO: /* valinta */
        FIND DMarketing WHERE RECID(DMarketing) = rtab[FRAME-LINE] NO-LOCK.
        siirto = STRING(DirMark).
        LEAVE LOOP.
     END.


     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        IF order = 2 THEN FIND FIRST DMarketing
        USE-INDEX DirMark  WHERE DMarketing.Brand = gcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 1 THEN FIND FIRST DMarketing
        USE-INDEX DirMarkName  WHERE DMarketing.Brand = gcBrand 
        NO-LOCK NO-ERROR.
        ASSIGN memory = RECID(DMarketing) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 2 THEN FIND LAST DMarketing
        USE-INDEX DirMark  WHERE DMarketing.Brand = gcBrand
        NO-LOCK NO-ERROR.
        ELSE IF order = 1 THEN FIND LAST DMarketing
        USE-INDEX DirMarkName  WHERE DMarketing.Brand = gcBrand
        NO-LOCK NO-ERROR.
        ASSIGN memory = RECID(DMarketing) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
       /* haettava = TRUE. */
        HIDE FRAME sel NO-PAUSE.
        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel  NO-PAUSE.
/*HIDE FRAME alku NO-PAUSE. */
si-recid = xrecid.

