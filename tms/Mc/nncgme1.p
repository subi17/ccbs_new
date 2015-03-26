/* -----------------------------------------------------------------
   MODULI .......: NNCGME1.P
   TEHTÄVÄ ......: Mebers in a Customer group
   SOVELLUS .....: nn
   TEKIJÄ .......: pt
   LUONTIPVM ....: 12-12-98
   MUUTOSPVM ....: 29.12.98 pt count members, gather members
                   25.09.02 new menutest to button 5
                   08.11.02 jr Eventlog
                   11.03.03 tk tokens
                   19.03.03 aam run tasks when changes occur (fecgtask)
                   16.09.03/aam brand
   VERSIO .......: M15
   -------------------------------------------------------------- */

{commali.i}
{eventval.i} 
{fecgtask.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'CGMember'}

DEF INPUT PARAMETER icCustGroup LIKE CustGroup.CustGroup NO-UNDO .

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF BUFFER xCustGroup FOR CustGroup.
DEF BUFFER cmember FOR CGMember.

DEF VAR CustNum  LIKE CGMember.CustNum  NO-UNDO.
DEF VAR CustName LIKE CGMember.CustName NO-UNDO.
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
DEF VAR Qty          AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"    NO-UNDO.
DEF VAR xcg-code     AS c                      NO-UNDO.
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
      RUN eventview2.p(lhCGMember).
   END.
END.

form
    CGMember.CustNum      /* COLUMN-LABEL FORMAT */
    CGMember.CustName     /* COLUMN-LABEL FORMAT */
    CGMember.Memo     /* COLUMN-LABEL FORMAT */
    Customer.PostOffice       column-label "City" format "x(12)"
WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    " Members in a c-group " + icCustGroup + " (" + gcBrand + ") "
   FRAME sel.

form
    CGMember.CustNum     /* LABEL FORMAT */
    Customer.CustName     /* LABEL FORMAT */
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    lm-ots WITH side-labels 1 columns
    FRAME lis.

form /* member :n haku kentällä CustNum */
    CustNum
    help "Type Customer number "
    with row 4 col 2 title color value(ctc) " FIND CUST. NO. "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* member :n haku kentällä CustName */
    CustName
    help "Type Customer's Name"
    with row 4 col 2 title color value(ctc) " FIND CUST. Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

form
   skip(1)
"  Note:  All members (that are NOT already members in this group) "
"         from group" xcg-code
help "Enter Code of the Group You want to copy" xCustGroup.CGName
"         are copied also into this customer group"
skip(1)
WITH
   centered row 5 overlay no-labels title " Copy members into group " +
   CustGroup.CustGroup + " " FRAME copy .


FIND CustGroup where 
     CustGroup.Brand     = gcBrand AND
     CustGroup.CustGroup = icCustGroup no-lock.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST CGMember 
OF CustGroup no-lock no-error.
IF AVAILABLE CGMember THEN ASSIGN
   muisti       = recid(CGMember)
   tulostettava = TRUE
   lisattava    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No group members available !" VIEW-AS ALERT-BOX.
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
       if jarj = 1 then put screen row 18 col 30 " By CustNo.  ".
       if jarj = 2 then put screen row 18 col 30 " By CustName ".
    END.


    IF lisattava THEN DO:
       lisattava = FALSE. ufkey = TRUE.

ADD-CUST:
       repeat TRANS ON ENDKEY UNDO ADD-CUST, LEAVE ADD-CUST.
          ASSIGN ufkey = TRUE ufk = 0 ehto = 0
          ufk[1] = 511 ufk[2] = 513 ufk[3] = 516 ufk[4] = 529 ufk[8] = 8.
          RUN ufkey.

          IF toimi = 8 THEN LEAVE ADD-CUST.
          IF toimi = 1 THEN DO:
             lm-ots = " ADD ONE CUSTOMER ".

             add-single:
             repeat WITH FRAME lis ON ENDKEY UNDO ADD-CUST,
                               NEXT ADD-CUST:
                PAUSE 0.
                ehto = 9. RUN ufkey.
                CLEAR FRAME lis no-pause.
                PROMPT-FOR CGMember.CustNum
                validate(INPUT CGMember.CustNum  = 0 OR 
                         can-find(FIRST Customer where
                                  Customer.Brand   = CustGroup.Brand AND
                                  Customer.CustNum = input CGMember.CustNum),
                        "Unknown customer !").
                IF INPUT CGMember.CustNum = 0 THEN DO:
                   HIDE FRAME lis.
                   UNDO, NEXT LOOP.
                END.

                FIND Customer where Customer.CustNum = INPUT CGMember.CustNum
                no-lock.
                DISP Customer.CustName.

                /* is this Customer already a member in this group ? */
                IF can-find(CGMember where 
                            CGMember.CustNum  = Customer.CustNum AND
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
                   muisti           = recid(CGMember)
                   tulostettava     = TRUE
                   CGMember.Brand     = CustGroup.Brand
                   CGMember.CustGroup = CustGroup.CustGroup
                   CGMember.CustNum  = Customer.CustNum
                   CGMember.CustName = Customer.CustName.
                   UPDATE CGMember.Memo.

                   lcTask = fECGEnterTask(TRUE).

                   IF llDoEvent 
                   THEN RUN StarEventMakeCreateEvent(lhCGMember).
                END.
             END.
          END. /* toimi = 1: add a single group */

          ELSE IF toimi = 2 THEN DO:
             RUN nncgcb(CustGroup.CustGroup).
             LEAVE add-cust.
          END.
          ELSE IF toimi = 3 THEN DO WITH FRAME copy:
             /* copy members */

             add-single:
             repeat WITH FRAME copy ON ENDKEY UNDO ADD-CUST: 

                PAUSE 0.
                                                                                            ehto = 9. RUN ufkey.
                CLEAR FRAME lis no-pause.
                PROMPT-FOR xcg-code
                validate(INPUT xcg-code  = "" OR 
                         can-find(xCustGroup where
                                  xCustGroup.Brand     = gcBrand AND
                                  xCustGroup.CustGroup = input xcg-code),
                         "Unknown group !").

                ASSIGN INPUT xcg-code.
                LEAVE add-single.
             END.

             if xcg-code ne "" THEN DO:
                FIND xCustGroup where 
                     xCustGroup.Brand     = gcBrand AND
                     xCustGroup.CustGroup = xcg-code no-lock.
                DISP xCustGroup.CGName.

                ok = FALSE.
                message "Are You SURE that You want to copy members (Y/N) ? "
                UPDATE ok.
                IF ok THEN DO:
                   message "Copying members into group " CustGroup "...".
                   i = 0.
                   FOR EACH cmember OF xCustGroup no-lock:

                       FIND CGMember where
                            CGMember.CustGroup = CustGroup.CustGroup  AND
                            CGMember.CustNum  = cmember.CustNum
                       no-lock no-error.

                       IF NOT AVAIL CGMember THEN DO:
                          CREATE CGMember.
                          ASSIGN
                          CGMember.Brand     = CustGroup.Brand
                          CGMember.CustNum   = cmember.CustNum
                          CGMember.CustGroup = CustGroup.CustGroup
                          CGMember.CustName  = cmember.CustName
                          i                  = i + 1 .

                          lcTask = fECGEnterTask(TRUE).

                          IF llDoEvent 
                          THEN RUN StarEventMakeCreateEvent(lhCGMember).
                       END.
                   END.
                   message "Totally" i "members were copied - press ENTER !".
                   PAUSE no-message.
                END.
                HIDE FRAME copy.
                LEAVE add-cust.
             END. 
             HIDE FRAME copy.
          END. /* toimi = 3 */
          ELSE IF toimi = 4 THEN DO:
             RUN nncggm(CustGroup.CustGroup).
             LEAVE add-cust.
          END.
      END. /* add-cust */

      /* onko yhtään tietuetta ? */
      FIND FIRST CGMember OF CustGroup no-lock no-error.
      IF NOT AVAILABLE CGMember THEN LEAVE LOOP.

      muisti = recid(CGMember).
      tulostettava = TRUE.
      NEXT LOOP.
   END.

tulostus:
   DO :
      IF tulostettava THEN DO:
        up FRAME-LINE - 1.
        FIND CGMember where recid(CGMember) = muisti no-lock no-error.

        /* tulostetaan 1 sivullinen tietoa ruudulle
        alkaen tietueesta, jonka avainarvo = muisti.
        alkaen rivilta privi */

        /* jos on juuri poistettu rivi, niin ... */
        IF privi > 0 THEN DOWN privi - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE CGMember THEN DO:
              FIND Customer where Customer.CustNum = CGMember.custnum no-lock.
              DISPLAY
              CGMember.CustNum CGMember.CustName 
              CGMember.Memo Customer.PostOffice

              /* sd */.
              rtab[FRAME-LINE] = recid(CGMember).
              IF jarj = 1 THEN FIND NEXT CGMember
              OF CustGroup no-lock no-error.
              ELSE IF jarj = 2 THEN FIND NEXT CGMember USE-INDEX CustName
              OF CustGroup no-lock no-error.
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
        ufk[1]= 702  ufk[2]= 30 ufk[3]= 530 ufk[4]= 518
        ufk[5]= (IF lcRight = "RW" THEN 1499 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4    ELSE 0)
        ufk[7]= 185 ufk[8]=   8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
        CHOOSE ROW CGMember.CustNum ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CGMember.CustNum WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
        CHOOSE ROW CGMember.CustName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CGMember.CustName WITH FRAME sel.
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
        FIND CGMember where recid(CGMember) = muisti NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           IF jarj = 1 THEN FIND prev CGMember
           OF CustGroup no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
           OF CustGroup no-lock no-error.
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
           OF CustGroup no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
           OF CustGroup no-lock no-error.
           IF NOT AVAILABLE CGMember THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* edellinen loytyi */
              scroll DOWN.
              FIND Customer Where Customer.CustNum = CGMember.CustNum no-lock.
              DISPLAY
              CGMember.CustNum CGMember.CustName CGMember.Memo 
              Customer.PostOffice.
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
           OF CustGroup no-lock no-error.
           ELSE IF jarj = 2 THEN FIND NEXT CGMember USE-INDEX CustName
           OF CustGroup no-lock no-error.
           IF NOT AVAILABLE CGMember THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* loytyi viela seuraava tietue */
              scroll up.
              FIND Customer where Customer.CustNum = CGMember.CustNum no-lock.
              DISPLAY
              CGMember.CustNum CGMember.CustName CGMember.Memo 
              Customer.PostOffice.
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
        OF CustGroup no-lock no-error.
        ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
        OF CustGroup no-lock no-error.
        IF AVAILABLE CGMember THEN DO:
           muisti = recid(CGMember).

           /* mennään tiedostoa taaksepäin 1 sivun verran */
           DO rivi = 1 TO (FRAME-DOWN - 1):
              IF jarj = 1 THEN FIND prev CGMember
              OF CustGroup no-lock no-error.
              ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
              OF CustGroup no-lock no-error.
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
       cfc = "puyr". RUN ufcolor.
       CustNum = 0.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE CustNum WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       IF CustNum <> 0 THEN DO:
          FIND FIRST CGMember OF CustGroup where
                     CGMember.CustNum >= CustNum 
                     no-lock no-error.
          IF NOT AVAILABLE CGMember THEN DO:
             BELL.
             message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.
          /* joku cgmember/CustNum loytyi */
          ASSIGN jarj = 1 muisti = recid(CGMember) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       CustName = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE CustName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if CustName <> "" THEN DO:
          FIND FIRST CGMember OF CustGroup USE-INDEX CustName where
                     CGMember.CustName >= CustName 
          no-lock no-error.
          IF NOT AVAILABLE CGMember THEN DO:
             bell. message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.
          /* joku cgmember/cu-name loytyi */
          ASSIGN jarj = 2 muisti = recid(CGMember) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"4,f4") > 0 THEN DO:  /* other memberships */
        FIND CGMember where recid(CGMember) = rtab[FRAME-LINE] no-lock.
        RUN nncgme2(CGMember.CustNum).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* ADD */
        lisattava = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"3,f3") > 0 THEN DO:  /* count # of members */

        ASSIGN Qty = 0 ehto = 3 ufkey = TRUE ufk = 0. RUN ufkey.
        message "Calculating no. of members, wait a moment please ...".
        FOR EACH CGMember OF CustGroup no-lock:
           Qty = Qty + 1.
        END.
        PAUSE 0.
        message "There are totally" Qty "members in this group - press ENTER !".
        PAUSE no-message.
        NEXT LOOP.
     END.
     else if lookup(nap,"f7,7") > 0 THEN REPEAT WITH FRAME SEL:

        FIND CGMember where recid(CGMember) = rtab[FRAME-LINE] no-lock.

        ASSIGN
           ufkey = TRUE
           ufk   = 0
           ehto  = 1
           ufk[1] = 1883 ufk[2] = 1888 
           ufk[4] = 0 ufk[5]= 0 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8.
        run ufkey.   

        IF toimi = 8 THEN NEXT SELAUS.

        IF toimi = 1 THEN RUN commontt(CGMember.CustNum).

        IF toimi = 2 THEN RUN mobilett(CGMember.CustNum).

     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* poisto */
       privi = FRAME-LINE.
       FIND CGMember where recid(CGMember) = rtab[FRAME-LINE] no-lock.

       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       CGMember.CustNum CGMember.CustName CGMember.Memo Customer.PostOffice
       /* sd */.

       IF jarj = 1 THEN FIND NEXT CGMember
       OF CustGroup no-lock no-error.
       ELSE IF jarj = 2 THEN FIND NEXT CGMember USE-INDEX CustName
       OF CustGroup no-lock no-error.
       IF AVAILABLE CGMember THEN muisti = recid(CGMember).
       ELSE DO:
          /* luetaan takaisin poistettava */
          FIND CGMember where recid(CGMember) = rtab[FRAME-LINE] no-lock.
          /* sitten edellinen */
          IF jarj = 1 THEN FIND prev CGMember
          OF CustGroup no-lock no-error.
          ELSE IF jarj = 2 THEN FIND prev CGMember USE-INDEX CustName
          OF CustGroup no-lock no-error.
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
       CGMember.CustNum CGMember.CustName CGMember.Memo Customer.PostOffice
       /* sd */.
       IF ok THEN DO:

           lcTask = fECGLeaveTask(TRUE).

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCGMember).
           DELETE CGMember.

           /* poistettiinko viimeinen tietue ? */
           IF NOT can-find(FIRST CGMember
           OF CustGroup) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           tulostettava = TRUE.
           NEXT LOOP.
       END.
       ELSE privi = 0. /* ei poistettukaan */
     END. /* poisto */

     else if lookup(nap,"enter,return") > 0 AND lcRight = "RW" 
     THEN DO WITH FRAME sel TRANSAction:
       /* muutos */
       FIND CGMember where recid(CGMember) = rtab[frame-line(sel)]
       exclusive-lock.
       assign lm-ots = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCGMember).
       UPDATE CGMember.Memo.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCGMember).
       xrecid = recid(CGMember).
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST CGMember
       OF CustGroup no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST CGMember USE-INDEX CustName
       OF CustGroup no-lock no-error.
       ASSIGN muisti = recid(CGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST CGMember
       OF CustGroup no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST CGMember USE-INDEX CustName
       OF CustGroup no-lock no-error.
       ASSIGN muisti = recid(CGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

