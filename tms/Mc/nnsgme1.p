/* -----------------------------------------------------
   MODULI .......: NNSGME1.P
   TEHTÄVÄ ......: Mebers in a Salesman group
   SOVELLUS .....: nn
   TEKIJÄ .......: pt
   LUONTIPVM ....: 28-12-98
   MUUTOSPVM ....: 11-11-02 jr Eventlog
                   17.09.03/aam brand
   VERSIO .......: M15
   ----------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER SMGroup LIKE SMGroup.SmGroup NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF BUFFER xSMGroup FOR SMGroup.
DEF BUFFER cmember  FOR SMGMember.

DEF VAR Salesman   LIKE SMGMember.Salesman    NO-UNDO.
DEF VAR SmName     LIKE SMGMember.SmName      NO-UNDO.
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
def var ok           as log format "Yes/No"    NO-UNDO.
DEF VAR xg-code      AS c                      NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSMGMember AS HANDLE NO-UNDO.
   lhSMGMember = BUFFER SMGMember:HANDLE.
   RUN StarEventInitialize(lhSMGMember).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhSMGMember).
   END.
END.

form
    SMGMember.Salesman    
    SMGMember.SmName     column-label "Salesman's name"
WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    COLOR value(cfc) TITLE COLOR value(ctc)
    " Members in Salesman Group " + SMGroup + " (" + gcBrand + ") " 
FRAME sel.

form
    SMGMember.Salesman  
    SMGMember.SmName    
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    lm-ots WITH side-labels 1 columns
FRAME lis.

form /* member :n haku kentällä Salesman */
    Salesman
    help "Enter Salesman's code "
    with row 4 col 2 title color value(ctc) " FIND Salesman "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* member :n haku kentällä SmName */
    SmName
    help "Enter Salesman's Name"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

form
   skip(1)
"  Note:  All members (that are NOT already members in this group) "
"         from group" xg-code
help "Enter Code of the Salesman Group You want to copy" xSMGroup.SGName
"         are copied also into this Salesman group"
skip(1)
WITH
   centered row 5 overlay no-labels title " Copy members into group " +
   SMGroup.SmGroup + " " FRAME copy .


FIND SMGroup where 
     SMGroup.Brand   = gcBrand AND
     SMGroup.SmGroup = SMGroup no-lock.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST SMGMember 
OF SMGroup no-lock no-error.
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
       if jarj = 1 then put screen row 18 col 34 " By Code ".
       if jarj = 2 then put screen row 18 col 34 " By Name ".
    END.


    IF lisattava THEN DO:
       lisattava = FALSE. ufkey = TRUE.

ADD-SMAN:
     repeat TRANS ON ENDKEY UNDO ADD-SMAN, LEAVE ADD-SMAN.
     ASSIGN ufkey = TRUE ufk = 0 ehto = 0
     ufk[1] = 521 ufk[2] = 522 ufk[3] = 516 ufk[8] = 8.
     RUN Syst/ufkey.p.

     IF toimi = 8 THEN LEAVE ADD-SMAN.
     IF toimi = 1 THEN DO:
        lm-ots = " ADD ONE Salesman ".

    add-single:
    repeat WITH FRAME lis ON ENDKEY UNDO ADD-SMAN,
               NEXT ADD-SMAN:
      PAUSE 0.
      ehto = 9. RUN Syst/ufkey.p.
      CLEAR FRAME lis no-pause.
      PROMPT-FOR SMGMember.Salesman
      validate(input SMGMember.Salesman = "" OR 
               can-find(Salesman where
                        Salesman.Brand    = gcBrand AND
                        Salesman.Salesman = INPUT SMGMember.Salesman),
              "Unknown Salesman !").
      if input SMGMember.Salesman = "" THEN DO:
         HIDE FRAME lis.
         UNDO, NEXT LOOP.
      END.

      FIND Salesman where 
           Salesman.Brand    = gcBrand AND
           Salesman.Salesman = INPUT SMGMember.Salesman
      no-lock.
      DISP Salesman.SMName @ SMGMember.SmName.

      /* is this Salesman already a member in this group ? */
      IF can-find(SMGMember OF SMGroup where
             SMGMember.Salesman = Salesman.Salesman)
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
         muisti           = recid(SMGMember)
         tulostettava     = TRUE
         SMGMember.Brand    = SMGroup.Brand
         SMGMember.SmGroup  = SMGroup.SMGroup
         SMGMember.Salesman = Salesman.Salesman
         SMGMember.SmName   = Salesman.SmName.
         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSMGMember).
      END.
        END.
     END. /* toimi = 1: add a single group */

     ELSE IF toimi = 2 THEN DO:
        RUN Mc/nnsgsb.p(SMGroup.SmGroup).
        LEAVE ADD-SMAN.
     END.

     ELSE IF toimi = 3 THEN DO WITH FRAME copy:
        /* copy members */

        PAUSE 0.
        ufkey = true. xg-code = "".
        UPDATE xg-code 
        validate(input xg-code = "" OR 
                 can-find(xSMGroup where
                          xSMGroup.Brand   = gcBrand AND
                          xSMGroup.SmGroup = input xg-code),"Unknown group !").

        if xg-code ne "" THEN DO:
           FIND xSMGroup where 
                xSMGroup.Brand   = gcBrand AND
                xSMGroup.SmGroup = xg-code no-lock.
           DISP xSMGroup.SGName. 

           ok = FALSE.
           message "Are You SURE that You want to copy members (Y/N) ? "
           UPDATE ok.
           IF ok THEN DO:
              message "Copying members into group " SMGroup "...".
              i = 0.
              FOR EACH cmember OF xSMGroup no-lock:

                 FIND SMGMember OF SMGroup where
                      SMGMember.Salesman  = cmember.Salesman
                 no-lock no-error.

                 IF NOT AVAIL SMGMember THEN DO:
                    CREATE SMGMember.
                    ASSIGN
                    SMGMember.Brand    = SMGroup.Brand
                    SMGMember.Salesman = cmember.Salesman
                    SMGMember.SmGroup  = SMGroup.SMGroup
                    SMGMember.SmName   = cmember.SmName
                    i                  = i + 1 .
                    IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSMGMember).
                 END.
              END.
              message "Totally" i "members were copied - press ENTER !".
              PAUSE no-message.
           END.

           HIDE FRAME copy.
           LEAVE ADD-SMAN.

        END.   /* toimi = 3 */  
      END.
   END. /* ADD-SMAN */

     /* onko yhtään tietuetta ? */
     FIND FIRST SMGMember OF SMGroup no-lock no-error.

      IF NOT AVAILABLE SMGMember THEN LEAVE LOOP.
      muisti = recid(SMGMember).
      tulostettava = TRUE.
      NEXT LOOP.
    END.

tulostus:
   DO :
      IF tulostettava THEN DO:
   up FRAME-LINE - 1.
   FIND SMGMember where recid(SMGMember) = muisti no-lock no-error.

   /* tulostetaan 1 sivullinen tietoa ruudulle
   alkaen tietueesta, jonka avainarvo = muisti.
   alkaen rivilta privi */

   /* jos on juuri poistettu rivi, niin ... */
   IF privi > 0 THEN DOWN privi - 1.

   repeat WITH FRAME sel:
      IF AVAILABLE SMGMember THEN DO:
         DISPLAY SMGMember.Salesman SMGMember.SMName.
         rtab[FRAME-LINE] = recid(SMGMember).
         IF jarj = 1 THEN FIND NEXT SMGMember
         OF SMGroup no-lock no-error.
         ELSE IF jarj = 2 THEN FIND NEXT SMGMember USE-INDEX SmName
         OF SMGroup no-lock no-error.
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
   ufk[1]= 885 ufk[2]= 30 ufk[3]= 0 ufk[4]= 518
   ufk[5]= 5   ufk[6]= 4  ufk[7]= 0 ufk[8]=   8 ufk[9]= 1
   ehto = 3 ufkey = FALSE.
   RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
   CHOOSE ROW SMGMember.Salesman {Syst/uchoose.i} no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) SMGMember.Salesman WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
   CHOOSE ROW SMGMember.SmName {Syst/uchoose.i} no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) SMGmember.SmName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
   jarj = jarj + 1. IF jarj > jarjlkm THEN jarj = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
   jarj = jarj - 1. IF jarj = 0 THEN jarj = jarjlkm.
      END.

      IF jarj <> ed-jarj THEN DO:
   ASSIGN ekarivi = 0 muisti = rtab[FRAME-LINE].
   FIND SMGMember where recid(SMGMember) = muisti.
   DO i = 1 TO FRAME-LINE - 1:
      IF jarj = 1 THEN FIND prev SMGMember
      OF SMGroup no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
      OF SMGroup no-lock no-error.
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
      OF SMGroup no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
      OF SMGroup no-lock no-error.
      IF NOT AVAILABLE SMGMember THEN DO:
         message "YOU ARE ON THE FIRST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* edellinen loytyi */
         scroll DOWN.
         DISPLAY SMGMember.Salesman Smgmember.SmName
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
      OF SMGroup no-lock no-error.
      ELSE IF jarj = 2 THEN FIND NEXT SMGMember USE-INDEX SmName
      OF SMGroup no-lock no-error.
      IF NOT AVAILABLE SMGMember THEN DO:
         message "YOU ARE ON THE LAST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* loytyi viela seuraava tietue */
         scroll up.
         DISPLAY SMGMember.Salesman SMGMember.SmName.
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
   OF SMGroup no-lock no-error.
   ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
   OF SMGroup no-lock no-error.
   IF AVAILABLE SMGMember THEN DO:
      muisti = recid(SMGMember).

      /* mennään tiedostoa taaksepäin 1 sivun verran */
      DO rivi = 1 TO (FRAME-DOWN - 1):
         IF jarj = 1 THEN FIND prev SMGMember
         OF SMGroup no-lock no-error.
         ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
         OF SMGroup no-lock no-error.
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
       cfc = "puyr". RUN Syst/ufcolor.p.
       Salesman = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE Salesman WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if Salesman <> "" THEN DO:
     FIND FIRST SMGMember OF SMGroup where
           SMGMember.Salesman >= Salesman 
           no-lock no-error.
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

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       SmName = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE SmName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if SmName <> "" THEN DO:
     FIND FIRST SMGMember OF SMGroup USE-INDEX SmName where
           SMGMember.SmName >= SmName  
     no-lock no-error.
     IF NOT AVAILABLE SMGMember THEN DO:
        bell. message "NOT FOUND !".
        PAUSE 1 no-message.
        NEXT SELAUS.
     END.
     /* joku sgmember/sm-name loytyi */
     ASSIGN jarj = 2 muisti = recid(SMGMember) tulostettava = TRUE.
     NEXT LOOP.
       END.
     END. /* Haku sar. 2 */


     else if lookup(nap,"4,f4") > 0 THEN DO:  /* other memberships */
   FIND SMGMember where recid(SMGMember) = rtab[FRAME-LINE] no-lock.
   disp smgmember.
   RUN Mc/nnsgme2.p(SMGMember.Salesman).
   ufkey = TRUE.
   NEXT LOOP.
     END.

     else if lookup(nap,"5,f5") > 0 THEN DO:  /* other memberships */
   lisattava = TRUE.
   NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSAction:  /* poisto */
       privi = FRAME-LINE.
       FIND SMGMember where recid(SMGMember) = rtab[FRAME-LINE] no-lock.

       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       SMGMember.Salesman SMGMember.SmName.

       IF jarj = 1 THEN FIND NEXT SMGMember
       OF SMGroup no-lock no-error.
       ELSE IF jarj = 2 THEN FIND NEXT SMGMember USE-INDEX SmName
       OF SMGroup no-lock no-error.
       IF AVAILABLE SMGMember THEN muisti = recid(SMGMember).
       ELSE DO:
     /* luetaan takaisin poistettava */
     FIND SMGMember where recid(SMGMember) = rtab[FRAME-LINE] no-lock.
     /* sitten edellinen */
     IF jarj = 1 THEN FIND prev SMGMember
     OF SMGroup no-lock no-error.
     ELSE IF jarj = 2 THEN FIND prev SMGMember USE-INDEX SmName
     OF SMGroup no-lock no-error.
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
       SMGMember.Salesman SMGMember.SmName.
       IF ok THEN DO:
       IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSMGMember).
       DELETE SMGMember.

      /* poistettiinko viimeinen tietue ? */
      IF NOT can-find(FIRST SMGMember
      OF SMGroup) THEN DO:
         CLEAR FRAME sel no-pause.
         PAUSE 0 no-message.
         LEAVE LOOP.
      END.
      tulostettava = TRUE.
      NEXT LOOP.
       END.
       ELSE privi = 0. /* ei poistettukaan */
     END. /* poisto */
     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST SMGMember
       OF SMGroup no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST SMGMember USE-INDEX SmName
       OF SMGroup no-lock no-error.
       ASSIGN muisti = recid(SMGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST SMGMember
       OF SMGroup no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST SMGMember USE-INDEX SmName
       OF SMGroup no-lock no-error.
       ASSIGN muisti = recid(SMGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

