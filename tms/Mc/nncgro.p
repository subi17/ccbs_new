/*----- -----------------------------------------------
  MODULI .......: NNCGRO.P
  TEHTÄVÄ ......: Customer Groups
  SOVELLUS .....: NN
  TEKIJÄ .......: PT
  LUONTIPVM ....: 02-12-98
  MUUTOSPVM ....: 25-05-99 jp uright1 & uright2 added
                  21.05.02/tk invoice texts
                              Event logging added
                  11.03.03/tk tokens
                              invotxt tablename changed
                  18.03.03/tk RUN Mc/memo            
                  19.03.03 aam EnterTask & LeaveTask,
                               run tasks when changes occur (fecgtask)
                  27.03.03 tk  f4 in update closed program             
                  16.09.03/aam brand
                  06.02.04 jp  Custnum for memo
  VERSIO .......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable CustGroup

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CustGroup'}
{Syst/eventval.i}
{Func/fecgtask.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustGroup AS HANDLE NO-UNDO.
   lhCustGroup = BUFFER CustGroup:HANDLE.
   RUN StarEventInitialize(lhCustGroup).

   DEFINE VARIABLE lhCGMember AS HANDLE NO-UNDO.
   lhCGMember = BUFFER CGMember:HANDLE.
   RUN StarEventInitialize(lhCGMember).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhCustGroup).
   END.
END.

DEF BUFFER cmember FOR CGMember.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR CustGroup    LIKE CustGroup.CustGroup  NO-UNDO.
DEF VAR xcg-code     LIKE CustGroup.CustGroup  NO-UNDO.
DEF VAR CGName       LIKE CustGroup.CGName NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR ekarivi      AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orderlkm     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR privi        AS INT                    NO-UNDO  init 0.
DEF VAR ed-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
def var rivi         as int format "99"        NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR lisattava    AS LOG                    NO-UNDO.
DEF VAR lm-ots       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"    NO-UNDO.
DEF VAR lcTask       AS CHAR                   NO-UNDO. 

form
    CustGroup.Brand       COLUMN-LABEL "Brand" FORMAT "X(5)" 
    CustGroup.CustGroup      /* COLUMN-LABEL FORMAT */
    CustGroup.CGName      /* COLUMN-LABEL FORMAT */
    CustGroup.CreDate
    CustGroup.CreUser 
WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
       " Customer Groups " + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    CustGroup.CustGroup     /* LABEL FORMAT */
    CustGroup.CGName    /* LABEL FORMAT */
    CustGroup.EnterTask
    CustGroup.LeaveTask
    CustGroup.CreDate
    CustGroup.CreUser
    CustGroup.ChgDate
    CustGroup.ChgUser
    CustGroup.CreDate
    CustGroup.CreUser
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc) TITLE COLOR value(ctc) lm-ots WITH side-labels 1 columns
    FRAME lis.

{Func/brand.i}

form
   CustGroup.Memo
WITH
   centered 1 col no-label overlay row 2 
   title " Group '" + CustGroup.CustGroup +
         "' " + CustGroup.CGName + "  memo " FRAME memo.

form /* Customer Group :n haku kentällä CustGroup */
    "Brand:" lcBrand skip
    "Group:" CustGroup
    help "Type Group Code"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* Customer Group :n haku kentällä CGName */
    "Brand:" lcBrand skip
    "Name :" CGName
    help "Type first characters of a name"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST CustGroup
   WHERE CustGroup.Brand = lcBrand no-lock no-error.
IF AVAILABLE CustGroup THEN ASSIGN
   memory       = recid(CustGroup)
   must-print = TRUE
   lisattava    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No customer groups available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print = FALSE
      lisattava    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ed-order THEN DO:
       ed-order = order.
       if order = 1 then put screen row 19 col 35 " By Code ".
       if order = 2 then put screen row 19 col 35 " By Name ".
    END.

   IF lisattava THEN DO:  /* cgroupn lisäys  */
      assign cfc = "lis" ufkey = true lm-ots = " ADD " lisattava = FALSE.
      RUN Syst/ufcolor.

      lisaa:
      repeat WITH FRAME lis ON ENDKEY UNDO lisaa, LEAVE lisaa.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:
           PROMPT-FOR CustGroup.CustGroup
           VALIDATE
              (CustGroup.CustGroup = "" OR
              NOT can-find(CustGroup using  CustGroup.CustGroup WHERE
                           CustGroup.Brand = lcBrand),
              "Customer Group " + string(INPUT CustGroup.CustGroup) +
              " already exists !").
           if input CustGroup.CustGroup = "" THEN LEAVE lisaa.
           CREATE CustGroup.
           ASSIGN
           CustGroup.Brand       = lcBrand
           CustGroup.CustGroup   = INPUT FRAME lis CustGroup.CustGroup
           CustGroup.CreUser = katun.
           UPDATE CustGroup.CGName
                  CustGroup.EnterTask CustGroup.LeaveTask
                  CustGroup.CreDate CustGroup.CreUser.
           ASSIGN
           memory = recid(CustGroup)
           xrecid = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustGroup).

      END.  /* lisaa */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* onko yhtään tietuetta ? */
      FIND FIRST CustGroup
      WHERE CustGroup.Brand = lcBrand no-lock no-error.
      IF NOT AVAILABLE CustGroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

tulostus:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND CustGroup where recid(CustGroup) = memory no-lock no-error.

        /* tulostetaan 1 sivullinen tietoa ruudulle
        alkaen tietueesta, jonka avainarvo = memory.
        alkaen rivilta privi */

        /* jos on juuri poistettu rivi, niin ... */
        IF privi > 0 THEN DOWN privi - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE CustGroup THEN DO:
              DISPLAY 
                 CustGroup.Brand
                 CustGroup.CustGroup CustGroup.CGName
                 CustGroup.CreDate CustGroup.CreUser /* sd */.
              rtab[FRAME-LINE] = recid(CustGroup).
              IF order = 1 THEN FIND NEXT CustGroup
              WHERE CustGroup.Brand = lcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT CustGroup USE-INDEX CGName
              WHERE CustGroup.Brand = lcBrand no-lock no-error.
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
        ufk[1]= 35 ufk[2]= 30 ufk[3]= 927 ufk[4]= 510
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 1760   ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW CustGroup.CustGroup ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CustGroup.CustGroup WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CustGroup.CGName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) CustGroup.CGName WITH FRAME sel.
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
        ASSIGN ekarivi = 0 memory = rtab[FRAME-LINE].
        FIND CustGroup where recid(CustGroup) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev CustGroup
           WHERE CustGroup.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev CustGroup USE-INDEX CGName
           WHERE CustGroup.Brand = lcBrand no-lock no-error.
           IF AVAILABLE CustGroup THEN
              ASSIGN ekarivi = i memory = recid(CustGroup).
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
           FIND CustGroup where recid(CustGroup) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev CustGroup
           WHERE CustGroup.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev CustGroup USE-INDEX CGName
           WHERE CustGroup.Brand = lcBrand no-lock no-error.
           IF NOT AVAILABLE CustGroup THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* edellinen loytyi */
              scroll DOWN.
              DISPLAY CustGroup.Brand CustGroup.CustGroup CustGroup.CGName
                      CustGroup.CreDate CustGroup.CreUser /* sd */.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(CustGroup)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND CustGroup where recid(CustGroup) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT CustGroup
           WHERE CustGroup.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT CustGroup USE-INDEX CGName
           WHERE CustGroup.Brand = lcBrand no-lock no-error.
           IF NOT AVAILABLE CustGroup THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* loytyi viela seuraava tietue */
              scroll up.
              DISPLAY CustGroup.Brand CustGroup.CustGroup CustGroup.CGName
                      CustGroup.CreDate CustGroup.CreUser /* sd */.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(CustGroup).
              /* ja lopuksi pannaan memoryin ylimman rivin avain */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
        memory = rtab[1].
        FIND CustGroup where recid(CustGroup) = memory no-lock no-error.
        IF order = 1 THEN FIND prev CustGroup
        WHERE CustGroup.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND prev CustGroup USE-INDEX CGName
        WHERE CustGroup.Brand = lcBrand no-lock no-error.
        IF AVAILABLE CustGroup THEN DO:
           memory = recid(CustGroup).

           /* mennään tiedostoa taaksepäin 1 sivun verran */
           DO rivi = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev CustGroup
              WHERE CustGroup.Brand = lcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND prev CustGroup USE-INDEX CGName
              WHERE CustGroup.Brand = lcBrand no-lock no-error.
              IF AVAILABLE CustGroup THEN memory = recid(CustGroup).
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
           memory = rtab[FRAME-DOWN].
           FIND CustGroup where recid(CustGroup) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       CustGroup = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              CustGroup WITH FRAME f1.
       HIDE FRAME f1 no-pause.

       if CustGroup <> "" THEN DO:
          FIND FIRST CustGroup where 
                     CustGroup.Brand      = lcBrand AND
                     CustGroup.CustGroup >= CustGroup
          no-lock no-error.

          IF NOT fRecFound(1) THEN NEXT SELAUS.

          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       CGName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              CGName WITH FRAME f2.
       HIDE FRAME f2 no-pause.

       if CGName <> "" THEN DO:

          FIND FIRST CustGroup where 
                     CustGroup.Brand   = lcBrand AND  
                     CustGroup.CGName >= CGName
          USE-INDEX CGName no-lock no-error.

          IF NOT fRecFound(2) THEN NEXT SELAUS.

          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"3,f3") > 0 THEN DO:

       FIND CustGroup where recid(CustGroup) = rtab[frame-line(sel)]
       no-lock.
       RUN Mc/memo(INPUT 0,
                INPUT "CustGroup",
                INPUT STRING(CustGroup.CustGroup),
                INPUT "Customer group").
       ufkey = TRUE.
       NEXT LOOP.

     END.

     else if lookup(nap,"4,f4") > 0 THEN DO TRANSAction:  /* members */
        FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.
        RUN Mc/nncgme1(CustGroup.CustGroup).
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
       FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.

       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       CustGroup.CustGroup CustGroup.CGName CustGroup.CreDate CustGroup.CreUser.

       IF order = 1 THEN FIND NEXT CustGroup
       WHERE CustGroup.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT CustGroup USE-INDEX CGName
       WHERE CustGroup.Brand = lcBrand no-lock no-error.
       IF AVAILABLE CustGroup THEN memory = recid(CustGroup).
       ELSE DO:
          /* luetaan takaisin poistettava */
          FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.
          /* sitten edellinen */
          IF order = 1 THEN FIND prev CustGroup
          WHERE CustGroup.Brand = lcBrand no-lock no-error.
          ELSE IF order = 2 THEN FIND prev CustGroup USE-INDEX CGName
          WHERE CustGroup.Brand = lcBrand no-lock no-error.
          IF AVAILABLE CustGroup THEN DO:
             ASSIGN
             privi = privi - 1  /* koska poistetaan viimeinen */
             memory = recid(CustGroup).
          END.
       END.

       /* FIND takaisin poistettavaan riviin */
       FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       CustGroup.CustGroup CustGroup.CGName CustGroup.CreDate CustGroup.CreUser.
       IF ok THEN DO:


           IF CustGroup.LeaveTask NE "" THEN DO:
              ok = FALSE.
              MESSAGE "Task" CustGroup.LeaveTask "has been defined to be run"
                      "when members are removed from this group. Shall the"
                      "task be run for each member now ?"
              VIEW-AS ALERT-BOX
              QUESTION
              BUTTONS YES-NO
              SET ok.
           END.

           FOR EACH CGMember of CustGroup:

              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCGMember).

              IF ok THEN lcTask = fECGLeaveTask(TRUE). 

              DELETE CGMember.
           END.

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCustGroup).

           DELETE CustGroup.

           /* poistettiinko viimeinen tietue ? */
           IF NOT can-find(FIRST CustGroup
           WHERE CustGroup.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE privi = 0. /* ei poistettukaan */
     END. /* poisto */

     ELSE IF lookup(nap,"7,f7") > 0 THEN DO:
        FIND CustGroup WHERE recid(CustGroup) = rtab[FRAME-line(sel)] NO-LOCK.
        RUN Mc/invotxt("CustGroup",CustGroup.CustGroup).
        ASSIGN memory = recid(CustGroup) must-print = TRUE ufkey=true.
        NEXT LOOP.
     END.

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction ON ENDKEY UNDO, NEXT LOOP:
       /* muutos */
       {Syst/uright2.i}
       FIND CustGroup where recid(CustGroup) = rtab[frame-line(sel)] 
       exclusive-lock.

       assign lm-ots = " CHANGE " ufkey = TRUE ehto = 9.
       cfc = "lis". RUN Syst/ufcolor.
       DISPLAY CustGroup.CustGroup
       CustGroup.EnterTask CustGroup.LeaveTask
       CustGroup.CreUser CustGroup.CreDate
       CustGroup.ChgUser CustGroup.ChgDate
       CustGroup.CGName .

       IF lcRight = "RW" THEN DO:
          RUN Syst/ufkey.

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustGroup).

          UPDATE CustGroup.CGName
                 CustGroup.EnterTask
                 CustGroup.LeaveTask.

          ASSIGN
             CustGroup.ChgUser = katun
             CustGroup.ChgDate = TODAY.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustGroup).

       END.
       ELSE PAUSE.

       HIDE FRAME lis no-pause.

       DISPLAY CustGroup.CGName
               CustGroup.CreDate CustGroup.CreUser
       WITH FRAME sel.


       xrecid = recid(CustGroup).
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST CustGroup
       WHERE CustGroup.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST CustGroup USE-INDEX CGName
       WHERE CustGroup.Brand = lcBrand no-lock no-error.
       ASSIGN memory = recid(CustGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF order = 1 THEN FIND LAST CustGroup
       WHERE CustGroup.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST CustGroup USE-INDEX CGName
       WHERE CustGroup.Brand = lcBrand no-lock no-error.
       ASSIGN memory = recid(CustGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

