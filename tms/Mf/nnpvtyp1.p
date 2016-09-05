/* ----------------------------------------------------------------------------
  MODULE .......: NNPVTYP1.P
  FUNCTION .....: Product phonestatistics browsing
  APPLICATION ..: NN
  AUTHOR .......: pt
  CREATED ......: 13.02.02 lp modified from nnpvtyp.p, deleted ALL unnecessery,
                              because Cubio don't need daily statistic only     
                              product statistic
                  26.02.02 lp more numbers for minut format "zzzzzz9"
                  07.04.03 tk converted to new table names, use temp-table
  MODIFIED .....:
  VERSION ......: M15
  -------------------------------------------------------------------------- */
{Syst/commali.i}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF NEW SHARED TEMP-TABLE nnpvti
   FIELD pt-tuno AS CHAR
   FIELD pt-min  AS INT
   FIELD pt-kpl  AS INT
   FIELD pt-mk   AS DEC
   INDEX pt-tuno AS UNIQUE pt-tuno.

DEF VAR haku-pt-tuno    LIKE nnpvti.pt-tuno       NO-UNDO.
DEF VAR xrecid          AS RECID                           INIT ?.
DEF VAR firstline       AS INT                    NO-UNDO  INIT 0.
DEF VAR ufkey           AS LOG                    NO-UNDO  INIT TRUE.
DEF VAR memory          AS RECID                  NO-UNDO.
DEF VAR line            AS INT FORMAT "99"        NO-UNDO.
DEF VAR rtab            AS RECID EXTENT 24        NO-UNDO.
DEF VAR i               AS INT                    NO-UNDO.
DEF VAR ok              AS LOG FORMAT "Yes/No"    NO-UNDO.
DEF VAR pvm1            AS DA  FORMAT "99-99-99"  NO-UNDO.
DEF VAR pvm2            AS DA  FORMAT "99-99-99"  NO-UNDO.
DEF VAR tunimi          AS C                      NO-UNDO.

form /* Browsing list */
   nnpvti.pt-tuno     COLUMN-LABEL "Product Code" FORMAT "x(16)"
   HELP "Product's Code" 
   tunimi             COLUMN-LABEL "Product Name" FORMAT "x(28)"   
   nnpvti.pt-kpl      COLUMN-LABEL "   Qty   "    FORMAT "zzzzz9"
   HELP "Amount calls"
   nnpvti.pt-min      COLUMN-LABEL "Minutes"      FORMAT "zzzzzz9"
   nnpvti.pt-mk       COLUMN-LABEL "Sum (EUR)"    FORMAT "ZZZ,ZZZ,ZZ9.99"
   WITH WIDTH 80 OVERLAY SCROLL 1 15 DOWN COLOR VALUE(cfc)
   TITLE COLOR VALUE(ctc) " " + ynimi +
   " PRODUCT PHONESTATISTICS "  + STRING(pvm1,"99.99.99") + " - " + 
                                  STRING(pvm2,"99.99.99")                      
FRAME sel.

form /* Statistics search WITH FIELD pt-tuno */
   haku-pt-tuno
   HELP "Give Productcode"
   WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND PRODUCT "
   COLOR VALUE(cfc) NO-LABELS OVERLAY 
FRAME haku.

form /* Ask period */
   SKIP(1)
   " Note: Give dates for creating / browsing PRODUCT call statistic. "
   SKIP(1)
   "Time period ...:"  AT 9
   pvm1 NO-LABEL HELP "First date to calculate"
   "-"
   pvm2 NO-LABEL HELP "Last date to calculate"
   SKIP(1)
   WITH ROW 6 CENTERED TITLE COLOR VALUE(ctc) " PRODUCT STATISTICS "
   COLOR VALUE(cfc) NO-LABELS OVERLAY 
FRAME add-new.

ufkey = TRUE.
ehto = 9. RUN Syst/ufkey.p.

FIND FIRST FixCDR NO-LOCK.
ASSIGN pvm1 = FixCDR.Date
       pvm2 = TODAY - 1.

UPDATE pvm1 pvm2 validate (input pvm2  >= input pvm1,"Invalid order !")
WITH FRAME add-new.

ASSIGN
cfc      = "Lis"
ufkey    = TRUE.
RUN Syst/ufcolor.p.

ASSIGN
ok = FALSE.
MESSAGE " ARE YOU SURE YOU WANT TO START (Y/N) ? " UPDATE ok.
HIDE FRAME add-new.
IF NOT ok THEN RETURN.

ehto = 5.
RUN Syst/ufkey.p.

RUN nnptla1(INPUT pvm1, INPUT pvm2).

FIND FIRST nnpvti NO-LOCK NO-ERROR.
ASSIGN memory = RECID(nnpvti).
UP FRAME-LINE WITH FRAME sel. 

LOOP:
REPEAT WITH FRAME sel:

   UP FRAME-LINE - 1.       
   FIND nnpvti WHERE RECID(nnpvti) = memory NO-LOCK NO-ERROR.

   /* print 1 page data on the screen
   beginning from the record whose keyvalue = memory   */

   REPEAT WITH FRAME sel:
      IF AVAILABLE nnpvti THEN DO:
         IF pt-tuno = "TOTAL" THEN tunimi = "". ELSE DO:
            FIND BillItem WHERE BillCode = pt-tuno NO-LOCK NO-ERROR.
            IF AVAIL BillItem THEN tunimi = BillItem.BIName.
            ELSE tunimi = "!! Unknown !!".
         END.
         DISPLAY nnpvti.pt-tuno nnpvti.pt-kpl 
                 nnpvti.pt-min nnpvti.pt-mk tunimi.
         rtab[FRAME-LINE] = RECID(nnpvti).
         FIND NEXT nnpvti USE-INDEX pt-tuno 
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
   ASSIGN firstline = 0.
   PAUSE 0 NO-MESSAGE.

   /* one page of data has been printed AND
   the cursor is in the upmost line FOR 'choose' */
BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:
   PUT SCREEN ROW 19 COL 34 "  By Product ".
      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 703 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 0   ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         {Syst/uright1.i '"5,6"'}
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
         CHOOSE ROW nnpvti.pt-tuno {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) nnpvti.pt-tuno WITH FRAME sel.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = KEYLABEL(LASTKEY).

      /* Previous line */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:

         IF FRAME-LINE = 1 THEN DO:
            FIND nnpvti WHERE RECID(nnpvti) = rtab[1] NO-LOCK.
            FIND PREV nnpvti USE-INDEX pt-tuno 
             NO-LOCK NO-ERROR.
            IF NOT AVAILABLE nnpvti THEN DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL. 
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               SCROLL DOWN.
               IF pt-tuno = "TOTAL" THEN tunimi = "". ELSE DO:
                  FIND BillItem WHERE BillCode = pt-tuno NO-LOCK NO-ERROR.
                  IF AVAIL BillItem THEN tunimi = BillItem.BIName.
                  ELSE tunimi = "!! Unknown !!".
               END.
               DISPLAY nnpvti.pt-tuno nnpvti.pt-kpl nnpvti.pt-min
                       nnpvti.pt-mk tunimi.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = RECID(nnpvti)
               memory = rtab[1].
            END.
         END.
         ELSE UP 1.
      END. /* previous line */

      /* NEXT line */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND nnpvti WHERE RECID(nnpvti) = rtab[FRAME-DOWN] NO-LOCK .
            FIND NEXT nnpvti USE-INDEX pt-tuno 
             NO-LOCK NO-ERROR.
            IF NOT AVAILABLE nnpvti THEN DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL. 
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               SCROLL UP.
               IF pt-tuno = "TOTAL" THEN tunimi = "". ELSE DO:
                  FIND BillItem WHERE BillCode = pt-tuno NO-LOCK NO-ERROR.
                  IF AVAIL BillItem THEN tunimi = BillItem.BIName.
                  ELSE tunimi = "!! Unknown !!".
               END.
               DISPLAY nnpvti.pt-tuno nnpvti.pt-kpl 
                       nnpvti.pt-min nnpvti.pt-mk tunimi.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = RECID(nnpvti).
               /* finally LAST line's keyvalue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* Previous page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND nnpvti WHERE RECID(nnpvti) = memory NO-LOCK NO-ERROR.
         FIND PREV nnpvti USE-INDEX pt-tuno 
          NO-LOCK NO-ERROR.
         IF AVAILABLE nnpvti THEN DO:
            memory = RECID(nnpvti).

            /* go back one page */
            DO LINE = 1 TO (FRAME-DOWN - 1):
               FIND PREV nnpvti USE-INDEX pt-tuno 
                NO-LOCK NO-ERROR.
               IF AVAILABLE nnpvti THEN memory = RECID(nnpvti).
               ELSE line = FRAME-DOWN.
            END.
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
      ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
         /* cursor TO the downmost line */
         IF rtab[FRAME-DOWN] = ? THEN DO:
            MESSAGE "YOU ARE ON THE LAST PAGE !".
            BELL. 
            PAUSE 1 NO-MESSAGE.
         END.
         ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND nnpvti WHERE RECID(nnpvti) = memory NO-LOCK.
            NEXT LOOP.
         END.
      END. /* NEXT page */

      /* Search column 1 */
      ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
         cfc = "puyr". RUN Syst/ufcolor.p.
         haku-pt-tuno = "".
         ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
         UPDATE haku-pt-tuno WITH FRAME haku.
         HIDE FRAME haku NO-PAUSE.
         IF haku-pt-tuno <> "" THEN DO:
            FIND FIRST nnpvti USE-INDEX pt-tuno WHERE 
            nnpvti.pt-tuno >= haku-pt-tuno NO-LOCK NO-ERROR.
            IF NOT AVAILABLE nnpvti THEN DO:
               BELL. 
               MESSAGE "NONE FOUND !".
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            /*  nnpvti.pt-tuno was found */
            ASSIGN  
            memory = RECID(nnpvti). 
            NEXT LOOP.
         END.
      END. /* Haku sar. 1 */

      ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
         FIND FIRST nnpvti USE-INDEX pt-tuno 
          NO-LOCK NO-ERROR.
         ASSIGN memory = RECID(nnpvti). 
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
         FIND LAST nnpvti USE-INDEX pt-tuno 
          NO-LOCK NO-ERROR. 
         ASSIGN memory = RECID(nnpvti). 
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

   END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

