/* --------------------------------------------------------
  MODULI .......: NNCGME2.P
  TEHTÄVÄ ......: Memberships of a single Customer
  SOVELLUS .....: nn
  TEKIJÄ .......: pt
  LUONTIPVM ....: 12-12-98
  MUUTOSPVM ....: 08-11-02 jr Eventlog
                  11.03.03 tk tokens
                  19.03.03 aam run tasks when changes occur (fecgtask)
                  16.09.03/aam brand
  VERSIO .......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 
{Func/fecgtask.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CGMember'}

DEF INPUT PARAMETER CustNum LIKE Customer.CustNum NO-UNDO.

DEF BUFFER xCustomer FOR Customer.
DEF BUFFER cmember FOR CGMember.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR CustGroup LIKE CGMember.CustGroup NO-UNDO.
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
DEF VAR xCustNum     LIKE Customer.CustNum     NO-UNDO.
DEF VAR lcTask       AS CHAR                   NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCGMember AS HANDLE NO-UNDO.
   lhCGMember = BUFFER CGMember:HANDLE.
   RUN StarEventInitialize(lhCGMember).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhCGMember).
   END.
END.

form
    CGMember.CustGroup     /* COLUMN-LABEL FORMAT */
    CustGroup.CGName     /* COLUMN-LABEL FORMAT */
    CGMember.Memo     /* COLUMN-LABEL FORMAT */
WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    " Memberships of CustNo " + string(CustNum) + ": " +
    substring(Customer.CustName,1,16)
    + " " FRAME sel.

form
    CGMember.CustGroup    label "GroupCode"
       help "Enter now a Customer Group Code"
    CustGroup.CGName NO-LABEL                        SKIP
    CGMember.Memo    label "Info ...."
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    title color value(ctc) " ADD ONE GROUP "
    WITH side-labels
    FRAME lis.

form
    CustGroup.Memo
WITH
    no-label 1 col overlay row 2 centered title " memo OF GROUP " +
    CustGroup.CustGroup + ": " + CustGroup.CGName + " " FRAME memo.

form /* member :n haku kentällä CustGroup */
    CustGroup
    help "Type Group Code "
with row 4 col 2 title color value(ctc) " FIND GROUP CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.


form
   skip(1)
"  Note:  All memberships (in groups where this Customer is NOT a member yet) "
"         from customer" xCustNum
help "Enter Customer number whose memberships You want to copy"
Customer.CustName
"         are copied also onto this customer"
skip(1)
WITH
   centered ROW 5 OVERLAY NO-LABELS TITLE
   " Copy memberships from another customer "
    FRAME copy .

FIND Customer where Customer.CustNum = CustNum no-lock.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST CGMember
where CGMember.CustNum = CustNum no-lock no-error.
IF AVAILABLE CGMember THEN ASSIGN
   muisti       = recid(CGMember)
   tulostettava = TRUE
   lisattava    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No memberships available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      muisti       = ?
      tulostettava = FALSE
      lisattava    = TRUE.
END.

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
          ufk[1] = 512 ufk[2] = 514 ufk[3] = 517
          ufk[8] = 8.
          RUN Syst/ufkey.

          IF toimi = 8 THEN LEAVE add-group.
          IF toimi = 1 THEN
add-single:
          repeat WITH FRAME lis ON ENDKEY UNDO add-group,
                            NEXT add-group:
             PAUSE 0.
             ehto = 9. RUN Syst/ufkey.
             CLEAR FRAME lis no-pause.
             PROMPT-FOR CGMember.CustGroup
             validate(input frame lis CGMember.CustGroup = "" OR
             can-find(CustGroup where
                      CustGroup.Brand     = Customer.Brand AND
                      CustGroup.CustGroup = INPUT FRAME lis CGMember.CustGroup),
                      "Unknown Group !").
             .
             if input CGMember.CustGroup = "" THEN DO:
                HIDE FRAME lis.
                UNDO, LEAVE add-group.
             END.

             FIND CustGroup where 
                  CustGroup.Brand     = Customer.Brand AND
                  CustGroup.CustGroup = INPUT CGMember.CustGroup
             no-lock.
             DISP CustGroup.CGName.

             /* is this Customer already a member in this group ? */
             IF can-find(CGMember where 
                         CGMember.CustNum  = CustNum AND
                         CGMember.CustGroup = CustGroup.CustGroup)
             THEN DO:
                 BELL.
                 message "This customer is already a member in this group !".
                 message "Press ENTER !".
                 PAUSE no-message.
                 NEXT add-single.
             END.
             ELSE DO:
                CREATE CGMember.
                ASSIGN
                muisti = recid(CGMember)
                tulostettava = TRUE
                INPUT FRAME lis CGMember.CustGroup
                CGMember.Brand    = Customer.Brand
                CGMember.CustNum  = CustNum
                CGMember.CustName = Customer.CustName.
                UPDATE CGMember.Memo.

                lcTask = fECGEnterTask(TRUE).

                IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCGMember).
             END.
          END. /* toimi = 1: add a single group */

          ELSE IF toimi = 2 THEN DO:
             RUN Mc/nncggb(Customer.CustNum).
             LEAVE add-group.
          END.

          ELSE IF toimi = 3 AND lcRight = "RW"
          THEN DO WITH FRAME copy:
             /* copy memberships from one Customer TO another Customer */


             PAUSE 0.
             CLEAR FRAME copy no-pause.
             ufkey = TRUE. xCustNum = 0.
             UPDATE xCustNum
                    validate(INPUT xCustNum = 0 OR 
                     can-find(xCustomer where
                              xCustomer.Brand   = Customer.Brand AND
                              xCustomer.CustNum = input xCustNum),
                    "Unknown Customer !").

             IF xCustNum NE 0 THEN DO:
                FIND xCustomer where xCustomer.CustNum = xCustNum no-lock.
                DISP xCustomer.CustName.

                ok = FALSE.
                MESSAGE
                "Are You SURE that You want to copy memberships (Y/N) ? "
                UPDATE ok.
                IF ok THEN DO:
                   MESSAGE
                   "Copying memberships from custNo. " xCustNum
                   "to custNo." CustNum.

                   i = 0.
                   FOR EACH cmember no-lock where
                            cmember.CustNum  = xCustNum:

                       FIND CGMember where
                            CGMember.CustGroup = cmember.CustGroup AND
                            CGMember.CustNum  = Customer.CustNum
                       no-lock no-error.

                       IF NOT AVAIL CGMember THEN DO:
                          CREATE CGMember.
                          ASSIGN
                          CGMember.Brand     = Customer.Brand
                          CGMember.CustNum   = Customer.CustNum
                          CGMember.CustGroup = cmember.CustGroup
                          CGMember.CustName  = Customer.CustName
                          i                  = i + 1.

                          lcTask = fECGEnterTask(TRUE).

                          IF llDoEvent 
                          THEN RUN StarEventMakeCreateEvent(lhCGMember).
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

       FIND FIRST CGMember where CGMember.CustNum = CustNum no-lock no-error.
       IF NOT AVAIL CGMember THEN LEAVE LOOP.

       ASSIGN
       tulostettava = TRUE
       muisti = recid(CGMember).
       NEXT LOOP.

    END.

tulostus:
   DO :
      IF tulostettava THEN DO:
        up FRAME-LINE - 1.
        FIND CGMember where recid(CGMember) = muisti no-lock no-error.

        /* tulostetaan 1 sivullinen tietoa ruudulle
        alkaen tietueesta, jonka avainarvo = muisti, alkaen rivilta privi */

        /* jos on juuri poistettu rivi, niin ... */
        IF privi > 0 THEN DOWN privi - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE CGMember THEN DO:
              FIND CustGroup of CGMember no-lock.
              DISPLAY CGMember.CustGroup CustGroup.CGName CGMember.Memo
                 /* sd */.
              rtab[FRAME-LINE] = recid(CGMember).
              IF jarj = 1 THEN FIND NEXT CGMember
              where CGMember.CustNum = CustNum no-lock no-error.
              ELSE IF jarj = 2 THEN FIND NEXT CGMember USE-INDEX CustName
              where CGMember.CustNum = CustNum no-lock no-error.
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
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7] = 528 ufk[8] = 8  ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
        CHOOSE ROW CGMember.CustGroup ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CGMember.CustGroup WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
        CHOOSE ROW CustGroup.CGName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CustGroup.CGName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).
      IF jarj <> ed-jarj THEN DO:
        ASSIGN ekarivi = 0 muisti = rtab[FRAME-LINE].
        FIND CGMember where recid(CGMember) = muisti.
        DO i = 1 TO FRAME-LINE - 1:
           IF jarj = 1 THEN FIND prev CGMember
           where CGMember.CustNum = CustNum no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
           where CGMember.CustNum = CustNum no-lock no-error.
           IF AVAILABLE CGMember THEN
              ASSIGN ekarivi = i muisti = recid(CGMember).
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
           FIND CGMember where recid(CGMember) = rtab[1] no-lock.
           IF jarj = 1 THEN FIND prev CGMember
           where CGMember.CustNum = CustNum no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
           where CGMember.CustNum = CustNum no-lock no-error.
           IF NOT AVAILABLE CGMember THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* edellinen loytyi */
              scroll DOWN.
              FIND CustGroup of CGMember no-lock.
              DISPLAY CGMember.CustGroup CustGroup.CGName CGMember.Memo
                      /* sd */.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(CGMember)
              muisti = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND CGMember where recid(CGMember) = rtab[FRAME-DOWN] no-lock .
           IF jarj = 1 THEN FIND NEXT CGMember
           where CGMember.CustNum = CustNum no-lock no-error.
           ELSE IF jarj = 2 THEN FIND NEXT CGMember USE-INDEX CustName
           where CGMember.CustNum = CustNum no-lock no-error.
           IF NOT AVAILABLE CGMember THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* loytyi viela seuraava tietue */
              scroll up.
              FIND CustGroup of CGMember no-lock.
              DISPLAY CGMember.CustGroup CustGroup.CGName CGMember.Memo
                      /* sd */.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(CGMember).
              /* ja lopuksi pannaan muistiin ylimman rivin avain */
              muisti = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
        muisti = rtab[1].
        FIND CGMember where recid(CGMember) = muisti no-lock no-error.
        IF jarj = 1 THEN FIND prev CGMember
        where CGMember.CustNum = CustNum no-lock no-error.
        ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
        where CGMember.CustNum = CustNum no-lock no-error.
        IF AVAILABLE CGMember THEN DO:
           muisti = recid(CGMember).

           /* mennään tiedostoa taaksepäin 1 sivun verran */
           DO rivi = 1 TO (FRAME-DOWN - 1):
              IF jarj = 1 THEN FIND prev CGMember
              where CGMember.CustNum = CustNum no-lock no-error.
              ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
              where CGMember.CustNum = CustNum no-lock no-error.
              IF AVAILABLE CGMember THEN muisti = recid(CGMember).
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
           FIND CGMember where recid(CGMember) = muisti no-lock.
           tulostettava = TRUE.
           NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       CustGroup = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE CustGroup WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if CustGroup <> "" THEN DO:
          FIND FIRST CGMember where
                     CGMember.CustGroup >= CustGroup AND
                     CGMember.CustNum = CustNum no-lock no-error.
          IF NOT AVAILABLE CGMember THEN DO:
             BELL.
             message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.
          /* joku cgmember/cg-code loytyi */
          ASSIGN jarj = 1 muisti = recid(CGMember) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */


     else if lookup(nap,"4,f4") > 0 THEN DO:  /* other members */
        FIND CGMember where recid(CGMember) = rtab[FRAME-LINE] no-lock.
        RUN Mc/nncgme1(CGMember.CustGroup).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        lisattava = TRUE.
        NEXT LOOP.
     END.


     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* poisto */
       privi = FRAME-LINE.
       FIND CGMember where recid(CGMember) = rtab[FRAME-LINE] no-lock.

       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       CGMember.CustGroup CustGroup.CGName CGMember.Memo /* sd */.

       IF jarj = 1 THEN FIND NEXT CGMember
       where CGMember.CustNum = CustNum no-lock no-error.
       ELSE IF jarj = 2 THEN FIND NEXT CGMember USE-INDEX CustName
       where CGMember.CustNum = CustNum no-lock no-error.
       IF AVAILABLE CGMember THEN muisti = recid(CGMember).
       ELSE DO:
          /* luetaan takaisin poistettava */
          FIND CGMember where recid(CGMember) = rtab[FRAME-LINE] no-lock.
          /* sitten edellinen */
          IF jarj = 1 THEN FIND prev CGMember
          where CGMember.CustNum = CustNum no-lock no-error.
          ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
          where CGMember.CustNum = CustNum no-lock no-error.
          IF AVAILABLE CGMember THEN DO:
             ASSIGN
             privi = privi - 1  /* koska poistetaan viimeinen */
             muisti = recid(CGMember).
          END.
       END.

       /* FIND takaisin poistettavaan riviin */
       FIND CGMember where recid(CGMember) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       CGMember.CustGroup CustGroup.CGName CGMember.Memo /* sd */.
       IF ok THEN DO:

           lcTask = fECGLeaveTask(TRUE).

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCGMember).
           DELETE CGMember.

           /* poistettiinko viimeinen tietue ? */
           IF NOT can-find(FIRST CGMember
           where CGMember.CustNum = CustNum) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           tulostettava = TRUE.
           NEXT LOOP.
       END.
       ELSE privi = 0. /* ei poistettukaan */
     END. /* poisto */

     else if lookup(nap,"7,f7") > 0 THEN DO:  /* other members */
        FIND CGMember where recid(CGMember) = rtab[FRAME-LINE] no-lock.
        FIND CustGroup of CGMember no-lock.
        PAUSE 0.
        DISP CustGroup.Memo WITH FRAME memo.
        ASSIGN ufk = 0 ufk[8] = 8 ehto = 0 ufkey = TRUE.  RUN Syst/ufkey.
        HIDE FRAME memo.
        NEXT LOOP.
     END.

     else if lookup(nap,"enter,return") > 0 AND lcRight = "RW"
     THEN DO WITH FRAME sel TRANSAction:
       /* muutos */
       FIND CGMember where recid(CGMember) = rtab[frame-line(sel)]
       exclusive-lock.
       assign lm-ots = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCGMember).
       UPDATE CGMember.Memo.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCGMember).
       xrecid = recid(CGMember).
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST CGMember
       where CGMember.CustNum = CustNum no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST CGMember USE-INDEX CustName
       where CGMember.CustNum = CustNum no-lock no-error.
       ASSIGN muisti = recid(CGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST CGMember
       where CGMember.CustNum = CustNum no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST CGMember USE-INDEX CustName
       where CGMember.CustNum = CustNum no-lock no-error.
       ASSIGN muisti = recid(CGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

