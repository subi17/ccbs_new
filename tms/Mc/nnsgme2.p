/* --------------------------------------------------------
  MODULI .......: NNSGME2.P
  TEHTÄVÄ ......: Memberships of a single Salesman
  SOVELLUS .....: nn
  TEKIJÄ .......: pt
  LUONTIPVM ....: 27-12-98
  MUUTOSPVM ....: 11-11-02 jr Eventlog
                  18.03.03 tk run memo
                  17.09.03/aam brand
                  06.02.04 jp custnum for memo
  VERSIO .......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER Salesman LIKE Salesman.Salesman NO-UNDO.

DEF BUFFER xSalesman FOR Salesman.
DEF BUFFER cmember FOR SMGMember.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR ekarivi      AS INT                    NO-UNDO  init 0.
DEF VAR jarj         AS INT                    NO-UNDO  init 1.
DEF VAR jarjlkm      AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR privi        AS INT                    NO-UNDO  init 0.
DEF VAR ed-jarj      AS INT                    NO-UNDO.
DEF VAR muisti       AS RECID                  NO-UNDO.
def var rivi         as int format "99"        NO-UNDO.
DEF VAR tulostettava AS LOG                    NO-UNDO.
DEF VAR lisattava    AS LOG                    NO-UNDO.
DEF VAR lm-ots       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"  NO-UNDO.
DEF VAR xSalesman     LIKE Salesman.Salesman       NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhSMGMember AS HANDLE NO-UNDO.
   lhSMGMember = BUFFER SMGMember:HANDLE.
   RUN StarEventInitialize(lhSMGMember).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhSMGMember)).
   END.
END.

form
    SMGMember.SmGroup     /* COLUMN-LABEL FORMAT */
    SMGroup.SGName     /* COLUMN-LABEL FORMAT */
WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    COLOR value(cfc) TITLE COLOR value(ctc)
    " Memberships of SMan " + string(Salesman) + ": " +
    substring(Salesman.SmName,1,16)
    + " " FRAME sel.

form
    SMGMember.SmGroup    label "GroupCode"
    help "Enter now a Salesman Group Code"
    SMGroup.SGName
WITH  OVERLAY ROW 4 centered
    color value(cfc) title color value(ctc) " ADD ONE GROUP "
    WITH 1 col FRAME lis.


form
    SMGroup.Memo
WITH
    no-label 1 col overlay row 2 centered title " memo OF GROUP " +
    SMGroup.SmGroup + ": " + SMGroup.SGName + " " FRAME memo.

form /* member :n haku kentällä Salesman */
    Salesman
    help "Type Group Code "
with row 4 col 2 title color value(ctc) " FIND GROUP CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.


form
   skip(1)
"  Note:  All memberships (in groups where this Salesman is NOT a member yet) "
"         from salesman" xSalesman
help "Enter Code of Salesman whose memberships You want to copy"
xSalesman.SmName
"         are copied also onto this salesman"
skip(1)
WITH
   centered ROW 5 OVERLAY NO-LABELS TITLE
   " Copy memberships from another Salesman "
    FRAME copy .

FIND Salesman where 
     Salesman.Brand    = gcBrand AND
     Salesman.Salesman = Salesman no-lock.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST SMGMember where 
           SMGMember.Brand = gcBrand AND 
           SMGMember.Salesman = Salesman no-lock no-error.
IF AVAILABLE SMGMember THEN ASSIGN
   muisti       = recid(SMGMember)
   tulostettava = TRUE
   lisattava    = FALSE.
ELSE ASSIGN
   muisti       = ?
   tulostettava = FALSE
   lisattava    = TRUE.

LOOP:
repeat WITH FRAME sel:

    IF jarj <> ed-jarj THEN DO:
       ed-jarj = jarj.
    END.

    IF lisattava THEN DO:
       lisattava = FALSE. ufkey = TRUE.

add-group:
       repeat TRANS ON ENDKEY UNDO add-group, LEAVE add-group.
          ASSIGN ufkey = TRUE ufk = 0 ehto = 0
          ufk[1] = 512 ufk[2] = 514 ufk[3] = 517 ufk[8] = 8.
          RUN ufkey.

          IF toimi = 8 THEN LEAVE add-group.
          IF toimi = 1 THEN
add-single:
          repeat WITH FRAME lis ON ENDKEY UNDO add-group,
                            NEXT add-group:
             PAUSE 0.
             ehto = 9. RUN ufkey.
             CLEAR FRAME lis no-pause.
             PROMPT-FOR SMGMember.SmGroup
             validate(input frame lis SMGMember.SmGroup = "" OR
             can-find(SMGroup where
                      SMGroup.Brand   = gcBrand AND
                      SMGroup.SmGroup = INPUT FRAME lis SMGMember.SmGroup),
                      "Unknown Group !").
             .
             if input SMGMember.SmGroup = "" THEN DO:
                HIDE FRAME lis.
                UNDO, LEAVE add-group.
             END.

             FIND SMGroup where 
                  SMGroup.Brand   = gcBrand AND
                  SMGroup.SmGroup = INPUT SMGMember.SmGroup
             no-lock.
             DISP SMGroup.SGName.

             /* is this Salesman already a member in this group ? */
             IF can-find(SMGMember where 
                         SMGMember.Brand = gcBrand AND 
                         SMGMember.Salesman = Salesman AND
                         SMGMember.SmGroup = SMGroup.SmGroup)
             THEN DO:
                 BELL.
                 message "This Salesman is already a member in this group !".
                 message "Press ENTER !".
                 PAUSE no-message.
                 NEXT add-single.
             END.
             ELSE DO:
                CREATE SMGMember.
                ASSIGN
                muisti = recid(SMGMember)
                tulostettava = TRUE
                INPUT FRAME lis SMGMember.SmGroup
                SMGMember.Brand    = gcBrand 
                SMGMember.Salesman = Salesman
                SMGMember.SmName   = Salesman.SmName.
                IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSMGMember).
             END.
          END. /* toimi = 1: add a single group */

          ELSE IF toimi = 2 THEN DO:
             RUN nnsggb(Salesman.Salesman).
             LEAVE add-group.
          END.

          ELSE IF toimi = 3 THEN DO WITH FRAME copy:
             /* copy memberships from one Salesman TO another Salesman */

             PAUSE 0.
             ufkey = true. xSalesman = "".
             UPDATE xSalesman
                    validate(input xSalesman = "" OR 
                             can-find(xSalesman where
                                      xSalesman.Brand    = gcBrand AND
                                      xSalesman.Salesman = input xSalesman),
                             "Unknown Salesman !").

             if xSalesman ne "" THEN DO:
                FIND xSalesman where 
                     xSalesman.Brand    = gcBrand AND
                     xSalesman.Salesman = xSalesman no-lock.
                DISP xSalesman.SmName.

                ok = FALSE.
                MESSAGE
                "Are You SURE that You want to copy memberships (Y/N) ? "
                UPDATE ok.
                IF ok THEN DO:
                   MESSAGE
                   "Copying memberships from Salesman" xSalesman
                   "to Salesman" Salesman.

                   i = 0.
                   FOR EACH cmember no-lock where
                            cmember.Brand     = gcBrand AND
                            cmember.Salesman  = xSalesman:

                       FIND SMGMember where
                            SMGMember.SmGroup  = cmember.SmGroup AND
                            SMGMember.Brand    = gcBrand AND 
                            SMGMember.Salesman = Salesman.Salesman
                       no-lock no-error.

                       IF NOT AVAIL SMGMember THEN DO:
                          CREATE SMGMember.
                          ASSIGN
                          SMGMember.Brand    = gcBrand 
                          SMGMember.Salesman = Salesman.Salesman
                          SMGMember.SmGroup  = cmember.SmGroup
                          SMGMember.SmName   = Salesman.SmName
                          i                  = i + 1.
                          IF llDoEvent 
                          THEN RUN StarEventMakeCreateEvent(lhSMGMember).
                       END.
                    END.

                    MESSAGE
                    "Totally" i "memberships were copied - press ENTER !".
                    PAUSE no-message.
                 END.
                 HIDE FRAME copy.
              END.
              LEAVE add-group.
          END.
       END. /* add-group */

       FIND FIRST SMGMember where 
                  SMGMember.Brand = gcBrand AND 
                  SMGMember.Salesman = Salesman 
       no-lock no-error.
       IF NOT AVAIL SMGMember THEN LEAVE LOOP.

       ASSIGN
       tulostettava = TRUE
       muisti = recid(SMGMember).
       NEXT LOOP.

    END.

tulostus:
   DO :
      IF tulostettava THEN DO:
        up FRAME-LINE - 1.
        FIND SMGMember where recid(SMGMember) = muisti no-lock no-error.

        /* tulostetaan 1 sivullinen tietoa ruudulle
        alkaen tietueesta, jonka avainarvo = muisti, alkaen rivilta privi */

        /* jos on juuri poistettu rivi, niin ... */
        IF privi > 0 THEN DOWN privi - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE SMGMember THEN DO:
              FIND SMGroup of SMGMember no-lock.
              DISPLAY SMGMember.SmGroup SMGroup.SGName
                 /* sd */.
              rtab[FRAME-LINE] = recid(SMGMember).
              IF jarj = 1 THEN FIND NEXT SMGMember
              where SMGMember.Brand = gcBrand AND 
                    SMGMember.Salesman = Salesman no-lock no-error.
              ELSE IF jarj = 2 THEN FIND NEXT SMGMember USE-INDEX SmName
              where SMGMember.Brand = gcBrand AND 
                    SMGMember.Salesman = Salesman no-lock no-error.
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
               tulostettava = FALSE.
        PAUSE 0 no-message.

        /* nyt on tulostettu 1 ruudullinen tavaraa ja kursori on ylim-
        mällä rivillä choosea varten. */
      END. /* tulostettava = TRUE */
   END. /* tulostus */

   /* jos on viimeksi poistettu rivi: */
   IF privi > 0 THEN DOWN privi - 1.
   ASSIGN privi = 0.

SELAUS:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35 ufk[2]= 0 ufk[3]= 0 ufk[4]= 519
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 528 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
        CHOOSE ROW SMGMember.SmGroup ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) SMGMember.SmGroup WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
        CHOOSE ROW SMGroup.SGName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) SMGroup.SGName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).
      IF jarj <> ed-jarj THEN DO:
        ASSIGN ekarivi = 0 muisti = rtab[FRAME-LINE].
        FIND SMGMember where recid(SMGMember) = muisti.
        DO i = 1 TO FRAME-LINE - 1:
           IF jarj = 1 THEN FIND prev SMGMember
           where SMGMember.Brand = gcBrand AND 
                 SMGMember.Salesman = Salesman no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
           where SMGMember.Brand = gcBrand AND 
                 SMGMember.Salesman = Salesman no-lock no-error.
           IF AVAILABLE SMGMember THEN
              ASSIGN ekarivi = i muisti = recid(SMGMember).
           ELSE LEAVE.
        END.
        tulostettava = TRUE.
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
           FIND SMGMember where recid(SMGMember) = rtab[1] no-lock.
           IF jarj = 1 THEN FIND prev SMGMember
           where SMGMember.Brand = gcBrand AND 
                 SMGMember.Salesman = Salesman no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
           where SMGMember.Brand = gcBrand AND 
                 SMGMember.Salesman = Salesman no-lock no-error.
           IF NOT AVAILABLE SMGMember THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* edellinen loytyi */
              scroll DOWN.
              FIND SMGroup of SMGMember no-lock.
              DISPLAY SMGMember.SmGroup SMGroup.SGName
                      /* sd */.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(SMGMember)
              muisti = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND SMGMember where recid(SMGMember) = rtab[FRAME-DOWN] no-lock .
           IF jarj = 1 THEN FIND NEXT SMGMember
           where SMGMember.Brand = gcBrand AND 
                 SMGMember.Salesman = Salesman no-lock no-error.
           ELSE IF jarj = 2 THEN FIND NEXT SMGMember USE-INDEX SmName
           where SMGMember.Brand = gcBrand AND 
                 SMGMember.Salesman = Salesman no-lock no-error.
           IF NOT AVAILABLE SMGMember THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* loytyi viela seuraava tietue */
              scroll up.
              FIND SMGroup of SMGMember no-lock.
              DISPLAY SMGMember.SmGroup SMGroup.SGName
                      /* sd */.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(SMGMember).
              /* ja lopuksi pannaan muistiin ylimman rivin avain */
              muisti = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
        muisti = rtab[1].
        FIND SMGMember where recid(SMGMember) = muisti no-lock no-error.
        IF jarj = 1 THEN FIND prev SMGMember
        where SMGMember.Brand = gcBrand AND 
              SMGMember.Salesman = Salesman no-lock no-error.
        ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
        where SMGMember.Brand = gcBrand AND 
              SMGMember.Salesman = Salesman no-lock no-error.
        IF AVAILABLE SMGMember THEN DO:
           muisti = recid(SMGMember).

           /* mennään tiedostoa taaksepäin 1 sivun verran */
           DO rivi = 1 TO (FRAME-DOWN - 1):
              IF jarj = 1 THEN FIND prev SMGMember
              where SMGMember.Brand = gcBrand AND 
                    SMGMember.Salesman = Salesman no-lock no-error.
              ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
              where SMGMember.Brand = gcBrand AND 
                    SMGMember.Salesman = Salesman no-lock no-error.
              IF AVAILABLE SMGMember THEN muisti = recid(SMGMember).
              ELSE rivi = FRAME-DOWN.
           END.
           tulostettava = TRUE.
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
           muisti = rtab[FRAME-DOWN].
           FIND SMGMember where recid(SMGMember) = muisti no-lock.
           tulostettava = TRUE.
           NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       Salesman = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE Salesman WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if Salesman <> "" THEN DO:
          FIND FIRST SMGMember where
                     SMGMember.Salesman >= Salesman AND
                     SMGMember.Brand = gcBrand AND 
                     SMGMember.Salesman = Salesman no-lock no-error.
          IF NOT AVAILABLE SMGMember THEN DO:
             BELL.
             message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.
          /* joku sgmember/sm-code loytyi */
          ASSIGN jarj = 1 muisti = recid(SMGMember) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */


     else if lookup(nap,"4,f4") > 0 THEN DO:  /* other members */
        FIND SMGMember where recid(SMGMember) = rtab[FRAME-LINE] no-lock.
        RUN nnsgme1(SMGMember.SmGroup).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        lisattava = TRUE.
        NEXT LOOP.
     END.


     else if lookup(nap,"6,f6") > 0 THEN DO TRANSAction:  /* poisto */
       privi = FRAME-LINE.
       FIND SMGMember where recid(SMGMember) = rtab[FRAME-LINE] no-lock.

       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       SMGMember.SmGroup SMGroup.SGName  /* sd */.

       IF jarj = 1 THEN FIND NEXT SMGMember
       where SMGMember.Brand = gcBrand AND 
             SMGMember.Salesman = Salesman no-lock no-error.
       ELSE IF jarj = 2 THEN FIND NEXT SMGMember USE-INDEX SmName
       where SMGMember.Brand = gcBrand AND 
             SMGMember.Salesman = Salesman no-lock no-error.
       IF AVAILABLE SMGMember THEN muisti = recid(SMGMember).
       ELSE DO:
          /* luetaan takaisin poistettava */
          FIND SMGMember where recid(SMGMember) = rtab[FRAME-LINE] no-lock.
          /* sitten edellinen */
          IF jarj = 1 THEN FIND prev SMGMember
          where SMGMember.Brand = gcBrand AND 
                SMGMember.Salesman = Salesman no-lock no-error.
          ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
          where SMGMember.Brand = gcBrand AND 
                SMGMember.Salesman = Salesman no-lock no-error.
          IF AVAILABLE SMGMember THEN DO:
             ASSIGN
             privi = privi - 1  /* koska poistetaan viimeinen */
             muisti = recid(SMGMember).
          END.
       END.

       /* FIND takaisin poistettavaan riviin */
       FIND SMGMember where recid(SMGMember) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       SMGMember.SmGroup SMGroup.SGName  /* sd */.
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSMGMember).
           DELETE SMGMember.

           /* poistettiinko viimeinen tietue ? */
           IF NOT can-find(FIRST SMGMember
           where SMGMember.Brand = gcBrand AND 
                 SMGMember.Salesman = Salesman) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           tulostettava = TRUE.
           NEXT LOOP.
       END.
       ELSE privi = 0. /* ei poistettukaan */
     END. /* poisto */
     else if lookup(nap,"7,f7") > 0 THEN DO:  /* memo */
        FIND SMGMember where recid(SMGMember) = rtab[FRAME-LINE] no-lock.
        FIND SMGroup of SMGMember no-lock.
        RUN memo(INPUT 0,
                 INPUT "SMGroup",
                 INPUT STRING(SMGroup.SMGroup),
                 INPUT "Salesman group").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST SMGMember
       where SMGMember.Brand = gcBrand AND 
             SMGMember.Salesman = Salesman no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST SMGMember USE-INDEX SmName
       where SMGMember.Brand = gcBrand AND 
             SMGMember.Salesman = Salesman no-lock no-error.
       ASSIGN muisti = recid(SMGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST SMGMember
       where SMGMember.Brand = gcBrand AND 
             SMGMember.Salesman = Salesman no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST SMGMember USE-INDEX SmName
       where SMGMember.Brand = gcBrand AND 
             SMGMember.Salesman = Salesman no-lock no-error.
       ASSIGN muisti = recid(SMGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

