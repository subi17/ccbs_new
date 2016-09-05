/* -----------------------------------------------
  MODULI .......: NNSGRO.P
  TEHTÄVÄ ......: Salesman Groups
  SOVELLUS .....: nn
  TEKIJÄ .......: pt
  LUONTIPVM ....: 18-12-98
  MUUTOSPVM ....: 25-05-99 jp uright1 & uright2 added
                  26.04.02/tk eventlogging added  
                  12.07.02/tk delete smgmembers on delete
                  25.02.03/tk tokens
                  18.03.03/tk RUN Mc/memo.p
                  17.09.03/aam brand
                  06.02.04 jp custnum for memo
  VERSIO .......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable SMGroup

{Syst/commali.i} 
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'smgroup'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSMGroup AS HANDLE NO-UNDO.
   lhSMGroup = BUFFER SMGroup:HANDLE.
   RUN StarEventInitialize(lhSMGroup).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhSMGroup).
   END.

END.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR SMGroup  LIKE SMGroup.SmGroup  NO-UNDO.
DEF VAR SGName LIKE SMGroup.SGName NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR ekarivi      AS INT                    NO-UNDO  init 0.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR orderlkm      AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR privi        AS INT                    NO-UNDO  init 0.
DEF VAR ed-order      AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
def var rivi         as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR lisattava    AS LOG                    NO-UNDO.
DEF VAR lm-ots       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"  NO-UNDO.

form
    SMGroup.Brand
    SMGroup.SmGroup 
    SMGroup.SGName  

WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Salesman Groups "
    + string(pvm,"99-99-99") + " "
FRAME sel.


form
   SMGroup.Memo
WITH
   centered 1 col no-label overlay row 2 title " Group '" + SMGroup.SmGroup +
   "' " + SMGroup.SGName + "  memo " 
FRAME memo.

form
    SMGroup.SmGroup
    SMGroup.SGName 
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    lm-ots WITH side-labels 1 columns
FRAME lis.

{Func/brand.i}

form /* Salesman group :n haku kentällä SMGroup */
    "Brand:" lcBrand skip
    "Group:" SMGroup
    help "Enter Code of Group"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* Salesman group :n haku kentällä SGName */
    "Brand:" lcBrand skip
    "Name :" SGName
    help "Enter Name of Group"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST SMGroup
WHERE SMGroup.Brand = lcBrand no-lock no-error.
IF AVAILABLE SMGroup THEN ASSIGN
   Memory       = recid(SMGroup)
   must-print = TRUE
   lisattava    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No salesman groups available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print = FALSE
      lisattava    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ed-order THEN DO:
       ed-order = order.
       if order = 1 then put screen row 19 col 36 " By Code ".
       if order = 2 then put screen row 19 col 36 " By Name ".
    END.

   IF lisattava THEN DO:  /* smgroupn lisäys  */
      assign cfc = "lis" ufkey = true lm-ots = " ADD " lisattava = FALSE.
      RUN Syst/ufcolor.p.
lisaa:
      repeat WITH FRAME lis ON ENDKEY UNDO lisaa, LEAVE lisaa.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.p.
        DO TRANSAction:
           PROMPT-FOR SMGroup.SmGroup
           VALIDATE
              (SMGroup.SmGroup = "" OR
              NOT can-find(SMGroup using  SMGroup.SmGroup WHERE
                           SMGroup.Brand = lcBrand),
              "salesman group " + string(INPUT SMGroup.SmGroup) +
              " already exists !").
           if input SMGroup.SmGroup = "" THEN LEAVE lisaa.
           CREATE SMGroup.
           ASSIGN
           SMGroup.SmGroup = INPUT FRAME lis SMGroup.SmGroup
           SMGroup.Brand   = lcBrand.
           UPDATE SMGroup.SGName
                  /* sd */
                  /* ld */.
           ASSIGN
           Memory = recid(SMGroup)
           xrecid = Memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSMGroup).

      END.  /* lisaa */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* onko yhtään tietuetta ? */
      FIND FIRST SMGroup
      WHERE SMGroup.Brand = lcBrand no-lock no-error.
      IF NOT AVAILABLE SMGroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

tulostus:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND SMGroup where recid(SMGroup) = Memory no-lock no-error.

        /* tulostetaan 1 sivullinen tietoa ruudulle
        alkaen tietueesta, jonka avainarvo = Memory.
        alkaen rivilta privi */

        /* jos on juuri poistettu rivi, niin ... */
        IF privi > 0 THEN DOWN privi - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE SMGroup THEN DO:
              DISPLAY SMGroup.Brand SMGroup.SmGroup SMGroup.SGName
                 /* sd */.
              rtab[FRAME-LINE] = recid(SMGroup).
              IF order = 1 THEN FIND NEXT SMGroup
              WHERE SMGroup.Brand = lcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT SMGroup USE-INDEX SGName
              WHERE SMGroup.Brand = lcBrand no-lock no-error.
           END.
           ELSE DO:
              CLEAR no-pause.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN ekarivi.
        ASSIGN ekarivi = 0
               must-print = FALSE.
        PAUSE 0 no-message.

        /* nyt on tulostettu 1 ruudullinen tavaraa ja kursori on ylim-
        mällä rivillä choosea varten. */
      END. /* must-print = TRUE */
   END. /* tulostus */

   /* jos on viimeksi poistettu rivi: */
   IF privi > 0 THEN DOWN privi - 1.
   ASSIGN privi = 0.

SELAUS:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 927 ufk[4]= 510
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {Syst/uright1.i '"3,5,6"'}
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW SMGroup.SmGroup {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) SMGroup.SmGroup WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SMGroup.SGName {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) SMGroup.SGName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > orderlkm THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = orderlkm.
      END.

      IF order <> ed-order THEN DO:
        ASSIGN ekarivi = 0 Memory = rtab[FRAME-LINE].
        FIND SMGroup where recid(SMGroup) = Memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev SMGroup
           WHERE SMGroup.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev SMGroup USE-INDEX SGName
           WHERE SMGroup.Brand = lcBrand no-lock no-error.
           IF AVAILABLE SMGroup THEN
              ASSIGN ekarivi = i Memory = recid(SMGroup).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT lisattava THEN DO:
        BELL.
        message "You are on an empty row, move upwards !".
        PAUSE 1 no-message.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* edellinen rivi */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND SMGroup where recid(SMGroup) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev SMGroup
           WHERE SMGroup.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev SMGroup USE-INDEX SGName
           WHERE SMGroup.Brand = lcBrand no-lock no-error.
           IF NOT AVAILABLE SMGroup THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* edellinen loytyi */
              scroll DOWN.
              DISPLAY SMGroup.Brand SMGroup.SmGroup SMGroup.SGName
                      /* sd */.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(SMGroup)
              Memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND SMGroup where recid(SMGroup) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT SMGroup
           WHERE SMGroup.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT SMGroup USE-INDEX SGName
           WHERE SMGroup.Brand = lcBrand no-lock no-error.
           IF NOT AVAILABLE SMGroup THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* loytyi viela seuraava tietue */
              scroll up.
              DISPLAY SMGroup.Brand SMGroup.SmGroup SMGroup.SGName.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(SMGroup).
              /* ja lopuksi pannaan Memoryin ylimman rivin avain */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
        Memory = rtab[1].
        FIND SMGroup where recid(SMGroup) = Memory no-lock no-error.
        IF order = 1 THEN FIND prev SMGroup
        WHERE SMGroup.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND prev SMGroup USE-INDEX SGName
        WHERE SMGroup.Brand = lcBrand no-lock no-error.
        IF AVAILABLE SMGroup THEN DO:
           Memory = recid(SMGroup).

           /* mennään tiedostoa taaksepäin 1 sivun verran */
           DO rivi = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev SMGroup
              WHERE SMGroup.Brand = lcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND prev SMGroup USE-INDEX SGName
              WHERE SMGroup.Brand = lcBrand no-lock no-error.
              IF AVAILABLE SMGroup THEN Memory = recid(SMGroup).
              ELSE rivi = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* sivun eka oli myos tiedoston eka */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* edellinen sivu */

     /* seuraava sivu */
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:
       /* kohdistin alimmalle riville */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* alin rivi ei ollut tyhja */
           Memory = rtab[FRAME-DOWN].
           FIND SMGroup where recid(SMGroup) = Memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       SMGroup = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              SMGroup WITH FRAME f1.
       HIDE FRAME f1 no-pause.

       if SMGroup <> "" THEN DO:
          FIND FIRST SMGroup where 
                     SMGroup.Brand    = lcBrand  AND
                     SMGroup.SmGroup >= SMGroup
          no-lock no-error.

          IF NOT fRecFound(1) THEN NEXT SELAUS.

          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       SGName = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              SGName WITH FRAME f2.
       HIDE FRAME f2 no-pause.

       if SGName <> "" THEN DO:
          FIND FIRST SMGroup where 
                     SMGroup.Brand = lcBrand AND
                     SMGroup.SGName >= SGName
          USE-INDEX SGName no-lock no-error.

          IF NOT fRecFound(2) THEN NEXT SELAUS.

          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"4,f4") > 0 THEN DO TRANSAction:  /* members */
        FIND SMGroup where recid(SMGroup) = rtab[FRAME-LINE] no-lock.
        RUN Mc/nnsgme1.p(SMGroup.SmGroup).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        {Syst/uright2.i}
        lisattava = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* poisto */
       {Syst/uright2.i}
       privi = FRAME-LINE.
       FIND SMGroup where recid(SMGroup) = rtab[FRAME-LINE] no-lock.

       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       SMGroup.SmGroup SMGroup.SGName /* sd */.

       IF order = 1 THEN FIND NEXT SMGroup
       WHERE SMGroup.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT SMGroup USE-INDEX SGName
       WHERE SMGroup.Brand = lcBrand no-lock no-error.
       IF AVAILABLE SMGroup THEN Memory = recid(SMGroup).
       ELSE DO:
          /* luetaan takaisin poistettava */
          FIND SMGroup where recid(SMGroup) = rtab[FRAME-LINE] no-lock.
          /* sitten edellinen */
          IF order = 1 THEN FIND prev SMGroup
          WHERE SMGroup.Brand = lcBrand no-lock no-error.
          ELSE IF order = 2 THEN FIND prev SMGroup USE-INDEX SGName
          WHERE SMGroup.Brand = lcBrand no-lock no-error.
          IF AVAILABLE SMGroup THEN DO:
             ASSIGN
             privi = privi - 1  /* koska poistetaan viimeinen */
             Memory = recid(SMGroup).
          END.
       END.

       /* FIND takaisin poistettavaan riviin */
       FIND SMGroup where recid(SMGroup) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       SMGroup.SmGroup SMGroup.SGName /* sd */.
       IF ok THEN DO:

           FOR EACH SMGMember OF SMGroup:
              DELETE SMGMember.
           END.


           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSMGroup).

           DELETE SMGroup.

           /* poistettiinko viimeinen tietue ? */
           IF NOT can-find(FIRST SMGroup
           WHERE SMGroup.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE privi = 0. /* ei poistettukaan */
     END. /* poisto */

     else if lookup(nap,"3,f3") > 0 THEN DO:    /* memo */
        FIND SMGroup where recid(SMGroup) = rtab[frame-line(sel)]
        no-lock.
        RUN Mc/memo.p(INPUT 0,
                 INPUT "SMGroup",
                 INPUT STRING(SMGroup.SMGroup),
                 INPUT "Salesman group").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
       /* muutos */
       {Syst/uright2.i}
       FIND SMGroup where recid(SMGroup) = rtab[frame-line(sel)]
       exclusive-lock.
       assign lm-ots = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p.
       DISPLAY 
         SMGroup.SmGroup
         SMGroup.SGName.

       IF lcRight = "RW" THEN DO:
          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSMGroup).

          UPDATE SMGroup.SGName.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSMGroup).
       END.
       ELSE PAUSE.
       HIDE FRAME lis no-pause.
       DISPLAY SMGroup.SGName
       WITH FRAME sel.
       xrecid = recid(SMGroup).

     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST SMGroup
       WHERE SMGroup.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST SMGroup USE-INDEX SGName
       WHERE SMGroup.Brand = lcBrand no-lock no-error.
       ASSIGN Memory = recid(SMGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF order = 1 THEN FIND LAST SMGroup
       WHERE SMGroup.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST SMGroup USE-INDEX SGName
       WHERE SMGroup.Brand = lcBrand no-lock no-error.
       ASSIGN Memory = recid(SMGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

