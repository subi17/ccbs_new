/* -----------------------------------------------
  MODULE .......: NNMAYP.P
  FUNCTION .....: maan nimien yllApito
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 25-04-96
  MODIFIED .....: 28.04.96 pt vaihdettu F1 ja F2 tekstit oikeinp{in
                  11.10.96 pt korjattu kielisyyksiA
                  25.06.98 kl vast => ok
                  17.05.99 jp uright1 & uright2 added
                  06.11.01 kl order = 3 into order = 2
                  26.04.02/tk eventlogging added
                  22.07.02 tk show full page on "end"
                  23.09.02/aam name as CCN,
                               show tariffs (F4)  
                  24.02.03/tk  user rights replaced with tokens
                  04.03.03/aam RepCCN added 
                  20.03.03/aam one parameter added for tariff.p
                  04.04.03 kl RUN Mc/tariff, new parameter
                  27.08.03/jp tariff needs 6 parameters
                  08.09.03/aam brand
                  16.10.03/aam report ccn removed
                  21.11.06/aam translations (invlang)
                  27.12.06 jp  ccn format 4 digit

  Version ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable CCN

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ccn'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCCN AS HANDLE NO-UNDO.
   lhCCN = BUFFER CCN:HANDLE.
   RUN StarEventInitialize(lhCCN).


   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCCN).
   END.
END.


DEF shared VAR siirto AS CHAR.

def var ok          as lo format "Yes/No"   NO-UNDO.
DEF VAR haku        LIKE CCN.CCN            NO-UNDO.
DEF VAR haku2       LIKE CCN.CCNName        NO-UNDO.
DEF VAR haku3       LIKE CCN.RepCCN         NO-UNDO.
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
DEF VAR endloop     AS INT                  NO-UNDO.

DEF BUFFER bCCN FOR CCN.

form
    CCN.Brand 
    CCN.CCN    /* column-label "LOpnr"          */ FORMAT ">>>9"
    CCN.CCNName   /* column-label "Country's name" */
    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi + " CALL CASE NUMBERS (CCN) "
    + string(pvm,"99-99-99") + " " FRAME sel.

{Func/brand.i}

form
    CCN.Brand    COLON 20 
    CCN.CCN      FORMAT ">>>9" COLON 20
    CCN.CCNName  COLON 20
    WITH  OVERLAY ROW 8 col 5
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 
    FRAME lis.

form /* nnmaan nimi :n tunnuksella hakua varten */
    "Brand:" lcBrand skip
    "CCN .:" haku
    help "Give CCN"
    with row 4 col 2 title color value(ctc) " FIND CCN"
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* nnmaan nimi :n nimella hakua varten */
    "Brand:" lcBrand skip
    "Name :" haku2
    help "Give CCN's Name or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

form /* report ccn  */
    "Brand :" lcBrand skip
    "RepCCN:" haku3
    help "Give reporting CCN"
    with row 4 col 2 title color value(ctc) " FIND REPORT CCN "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr3.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.
   FIND FIRST CCN WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
   IF AVAILABLE CCN THEN ASSIGN memory = recid(CCN)
      must-print = TRUE must-add    = FALSE.
   ELSE ASSIGN memory = ? must-print = FALSE must-add    = TRUE.
ASSIGN xrecid = ? delline = 0 ufkey = TRUE order = 1 firstline = 0.


LOOP:
    repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
            if order = 1 then put screen row 19 col 34 "   By CCN    ".
       else if order = 2 then put screen row 19 col 34 "   By Name   ".
    END.

   IF must-add THEN DO:  /* CCN -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN Syst/ufkey.
         DO TRANSAction:

            DISPLAY lcBrand @ CCN.Brand.

            PROMPT-FOR ccn.CCN           
            EDITING:
               READKEY.
               nap = keylabel(LASTKEY).
               IF lookup(nap,poisnap) > 0 THEN DO:
                  HIDE MESSAGE.
                  if frame-field = "ccn" THEN DO:
                     IF INPUT FRAME lis CCN = 0 THEN LEAVE add-new.
                     IF CAN-FIND(FIRST ccn WHERE 
                                CCN.Brand = lcBrand AND
                                ccn.ccn = INPUT ccn) THEN DO:
                        MESSAGE "CCN " + input ccn +
                                " already exists !". 
                        NEXT-PROMPT ccn.
                        NEXT.
                     END.
                  END.
               END.
               APPLY LASTKEY.
            END.


            CREATE CCN.
            ASSIGN
            CCN.Brand  = lcBrand 
            CCN.CCN    = INPUT FRAME lis CCN.CCN.
            UPDATE CCN.CCNName.

            ASSIGN
            memory = recid(CCN)
            xrecid = memory.
         END.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCCN).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST CCN WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CCN THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND CCN where recid(CCN) = memory NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE CCN THEN DO:
               DISPLAY 
                    CCN.Brand 
                    CCN.CCN CCN.CCNName /* CCN.RepCCN */
                        /* qq */.
               rtab[FRAME-LINE] = recid(CCN).
               IF order = 1 THEN FIND NEXT CCN 
                  WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
               ELSE IF order = 2 THEN FIND NEXT CCN USE-INDEX CCNName
                  WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
               ELSE IF order = 3 THEN FIND NEXT CCN USE-INDEX RepCCN
                  WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         IF endloop = 0 THEN up FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline = 0
                must-print = FALSE
                endloop = 0.
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
         ufk[1]= 35    ufk[2]= 30 
         ufk[3]= 814
         ufk[4]= 878
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW CCN.CCN ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) CCN.CCN WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW CCN.CCNName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) CCN.CCNName WITH FRAME sel.
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
         FIND CCN where recid(CCN) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND PREV CCN  
               WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 2 THEN FIND PREV CCN USE-INDEX CCNName
               WHERE CCN.Brand = lcBrand
               NO-LOCK NO-ERROR.
            ELSE IF order = 3 THEN FIND PREV CCN USE-INDEX RepCCN
               WHERE CCN.Brand = lcBrand
               NO-LOCK NO-ERROR.
            IF AVAILABLE CCN THEN
               ASSIGN firstline = i memory = recid(CCN).
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

      /* PREVious line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND CCN where recid(CCN) = rtab[1] NO-LOCK.
            IF order = 1 THEN FIND PREV CCN 
               WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 2 THEN FIND PREV CCN USE-INDEX CCNName
               WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 3 THEN FIND PREV CCN USE-INDEX RepCCN
               WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
            IF NOT AVAILABLE CCN THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a PREVious one was found */
               scroll DOWN.
               DISPLAY CCN.Brand CCN.CCN CCN.CCNName.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(CCN)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* PREVious line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND CCN where recid(CCN) = rtab[FRAME-DOWN] NO-LOCK .
            IF order = 1 THEN FIND NEXT CCN 
               WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 2 THEN FIND NEXT CCN USE-INDEX CCNName
               WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
            ELSE IF order = 3 THEN FIND NEXT CCN USE-INDEX RepCCN
               WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
            IF NOT AVAILABLE CCN THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               DISPLAY CCN.Brand CCN.CCN CCN.CCNName.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(CCN).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* PREVious page */
      else if lookup(nap,"PREV-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND CCN where recid(CCN) = memory NO-LOCK NO-ERROR.
         IF order = 1 THEN FIND PREV CCN 
            WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
         ELSE IF order = 2 THEN FIND PREV CCN USE-INDEX CCNName
            WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
         ELSE IF order = 3 THEN FIND PREV CCN USE-INDEX RepCCN
            WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
         IF AVAILABLE CCN THEN DO:
            memory = recid(CCN).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND PREV CCN 
                  WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
               ELSE IF order = 2 THEN FIND PREV CCN USE-INDEX CCNName
                  WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
               ELSE IF order = 3 THEN FIND PREV CCN USE-INDEX RepCCN
                  WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
               IF AVAILABLE CCN THEN memory = recid(CCN).
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
     END. /* PREVious page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND CCN where recid(CCN) = memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
        cfc = "puyr". RUN Syst/ufcolor.
        haku = 0.
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr.
        UPDATE lcBrand WHEN gcAllBrand
               haku WITH FRAME hayr.
        HIDE FRAME hayr no-pause.

        IF haku <> 0 THEN DO:
           FIND FIRST CCN where 
              CCN.Brand = lcBrand AND
              CCN.CCN >= INPUT haku
           NO-LOCK NO-ERROR.

           IF NOT fRecFound(1) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
        cfc = "puyr". RUN Syst/ufcolor.
        haku2 = "".
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr2.
        UPDATE lcBrand WHEN gcAllBrand
               haku2 WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.

        if haku2 <> "" THEN DO:

           FIND FIRST CCN where 
              CCN.Brand = lcBrand AND
              CCN.CCNName >= INPUT haku2
           NO-LOCK NO-ERROR.

           IF NOT fRecFound(2) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */

     /* translations */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 THEN DO:  
         FIND CCN WHERE RECID(CCN) = rtab[FRAME-LINE] NO-LOCK.
         RUN Mc/invlang(3,STRING(CCN.CCN)).
         
         ufkey = TRUE.
         NEXT LOOP.
     END.

     else if lookup(nap,"4,f4") > 0 THEN DO:  /* tariffs */
        FIND CCN where recid(CCN) = rtab[FRAME-LINE] NO-LOCK.

        RUN Mc/tariff(0,CCN.CCN,"",0,"",0). 

        UFKEY = TRUE.
        NEXT. 
     end. 

     else if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

         must-add = TRUE.
         NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */

        delline = FRAME-LINE.
        FIND CCN where recid(CCN) = rtab[FRAME-LINE] NO-LOCK.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) CCN.CCN CCN.CCNName.

        IF order = 1 THEN FIND NEXT CCN 
           WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 2 THEN FIND NEXT CCN USE-INDEX CCNName
           WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 3 THEN FIND NEXT CCN USE-INDEX RepCCN
           WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
        IF AVAILABLE CCN THEN memory = recid(CCN).

        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND CCN where recid(CCN) = rtab[FRAME-LINE] NO-LOCK.
           /* AND THEN the PREVious one */
           IF order = 1 THEN FIND PREV CCN
              WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND PREV CCN USE-INDEX CCNName
              WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
           ELSE IF order = 3 THEN FIND PREV CCN USE-INDEX RepCCN
              WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
           IF AVAILABLE CCN THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(CCN).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND CCN where recid(CCN) = rtab[FRAME-LINE]
        exclusive-lock.

        FOR FIRST BDest NO-LOCK WHERE 
                  BDest.Brand = CCN.Brand AND
                  BDest.CCN   = CCN.CCN:
           MESSAGE "This CCN is used as a reporting CCN for BDest" 
                   BDest.BDest
                   ". Deletion is not allowed."
           VIEW-AS ALERT-BOX.
           COLOR DISPLAY value(ccc) CCN.CCN CCN.CCNName.
           NEXT.
        END.

        FOR FIRST RateCCN NO-LOCK WHERE
                  RateCCN.Brand = CCN.Brand AND
                  RateCCN.CCN   = CCN.CCN:
           MESSAGE "This CCN is used as a rating CCN for BDest" 
                   RateCCN.BDest
                   ". Deletion is not allowed."
           VIEW-AS ALERT-BOX.
           COLOR DISPLAY value(ccc) CCN.CCN CCN.CCNName.
           NEXT.
        END.

        ASSIGN ok = FALSE.
        message " ARE YOU SURE YOU WANT TO REMOVE (Y/N)? " UPDATE ok.

        COLOR DISPLAY value(ccc) CCN.CCN CCN.CCNName.

        IF ok THEN DO:


            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCCN).

            DELETE CCN.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST CCN WHERE CCN.Brand = lcBrand) THEN DO:
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
        FIND CCN where recid(CCN) = rtab[frame-line(sel)]
        exclusive-lock.
        assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
        RUN Syst/ufkey.
        cfc = "lis". RUN Syst/ufcolor.
        DISPLAY CCN.Brand CCN.CCN .

        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCCN).
        IF lcRight = "RW" THEN
           UPDATE
              CCN.CCNName.
        ELSE DO:
           DISPLAY 
              CCN.CCNNAme.
           PAUSE.    
        end.       
        HIDE FRAME lis no-pause.
        DISPLAY CCN.CCNName
        WITH FRAME sel.

        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCCN).

        xrecid = recid(CCN).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST CCN
           WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 2 THEN FIND FIRST CCN USE-INDEX CCNName
           WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 3 THEN FIND FIRST CCN USE-INDEX RepCCN
           WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
        ASSIGN memory = recid(CCN) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST CCN 
           WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 2 THEN FIND LAST CCN USE-INDEX CCNName
           WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 3 THEN FIND LAST CCN USE-INDEX RepCCN
           WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.

        DO endloop = 1 TO FRAME-DOWN - 1:
           IF order = 1 THEN FIND PREV CCN
              WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND PREV CCN USE-INDEX CCNName
              WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
           ELSE IF order = 3 THEN FIND PREV CCN USE-INDEX RepCCN
              WHERE CCN.Brand = lcBrand NO-LOCK NO-ERROR.
        END.

        ASSIGN memory = recid(CCN) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

