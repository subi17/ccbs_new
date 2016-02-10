/* -----------------------------------------------
  MODULE .......: nnacyp.p
  FUNCTION .....: Browser of Account
  APPLICATION ..: TS
  AUTHOR .......: JP
  CREATED ......: 13-01-00 
  MODIFIED .....: 
                  13.11.01 lp  RUN Mc/memo added
                  30.01.02/aam shorter FORMAT FOR ac-type-name
                  26.04.02/tk  eventlogging added
                  05.03.03/tk  tokens
                  05.09.03/aam brand
                  06.02.04 jp  custnum for memo
                  12.02.04/aam AccNum to 6 digits
                  13.11.06/aam AccNum to 8 digits
                  29.03.07/aam get account type name from TMSCodes
  Version ......: M15 
------------------------------------------------------ */

&GLOBAL-DEFINE BrTable Account
{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'account'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhAccount AS HANDLE NO-UNDO.
   lhAccount = BUFFER Account:HANDLE.
   RUN StarEventInitialize(lhAccount).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhAccount). 
   END.

END.



DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ac-type-name  AS c                 NO-UNDO.
DEF VAR haku-ac-nr    LIKE Account.AccNum   NO-UNDO.
DEF VAR haku-ac-name  LIKE Account.AccName NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 2.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR must-add   AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.


FUNCTION paiv RETURNS CHARACTER(AccType AS INTEGER).
  ac-type-name = "".
  IF AccType > 0 THEN DO:
     ac-type-name = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                     "Account",                                
                                     "AccType",
                                     STRING(AccType)).
  END.  
  RETURN ac-type-name.
END.

form
    Account.Brand       
       FORMAT "x(5)" COLUMN-LABEL "Brand"   
    Account.AccNum       /* COLUMN-LABEL FORMAT */
       format ">>>>>>>9"
    Account.AccName     /* COLUMN-LABEL FORMAT */
    Account.AccType             /* COLUMN-LABEL FORMAT */
    ac-type-name        column-label "TypeName" format "x(25)"
    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " ACCOUNTS "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    Account.Brand    
    Account.AccNum     /* label format */ format ">>>>>>>9"
    Account.AccName   /* LABEL FORMAT */  FORMAT "X(45)"
    Account.AccType   /* LABEL FORMAT */
    ac-type-name         label "TypeName" format "x(30)"
    WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 1 columns
    FRAME lis.

{Func/brand.i}

form /* BROWSE search WITH FIELD Account */
    "Brand .:" lcBrand skip
    "Account:" haku-ac-nr FORMAT ">>>>>>>9"
    help "Give number of Account"
    with row 4 col 2 title color value(ctc) " FIND number "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

form /* BROWSE search WITH FIELD AccName */
    "Brand:" lcBrand skip
    "Name :" haku-ac-name
    help "Give Name of Account"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f2.

form /* memo */
WITH
    OVERLAY ROW 7 centered NO-LABEL
    color value(cfc) title color value(cfc) " Update memo "
    FRAME memo.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST Account WHERE Account.Brand = lcBrand
no-lock no-error.
IF AVAILABLE Account THEN ASSIGN
   memory       = recid(Account)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No account available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print = FALSE
      must-add    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 32 " Order by number ".
       if order = 2 then put screen row 19 col 32 " Order by Name   ".
    END.

   IF must-add THEN DO:  /* Account -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:

           DISPLAY lcBrand @ Account.Brand.

           PROMPT-FOR Account.AccNum
           VALIDATE
              (Account.AccNum = 0 OR
              NOT can-find(Account WHERE Account.brand = gcBrand
                           using  Account.AccNum),
              "BROWSE " + string(INPUT Account.AccNum) +
              " already exists !").
           IF INPUT Account.AccNum = 0 THEN LEAVE add-new.
           CREATE Account.
           ASSIGN
           Account.Brand  = lcBrand
           Account.AccNum = INPUT FRAME lis Account.AccNum.
           UPDATE Account.AccName
                  Account.AccType
           WITH FRAME lis
           EDITING:

             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
             THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "AccType" THEN DO:
                /* Checks names of Account type */
                ac-type-name = paiv(INPUT FRAME lis AccType).
                IF ac-type-name = "" THEN DO:
                   BELL.
                   MESSAGE "Unknown type !".
                   NEXT.
                END.
                DISP ac-type-name.
             END.
          END.
          APPLY LASTKEY.
       END. /* EDITING */

           ASSIGN
           memory = recid(Account)
           xrecid = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhAccount).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST Account WHERE Account.Brand = lcBrand
      no-lock no-error.
      IF NOT AVAILABLE Account THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND Account where recid(Account) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE Account THEN DO:
              /* Checks names of Account type */
              ac-type-name = paiv(AccType).

              DISPLAY Account.Brand Account.AccNum Account.AccName
                 AccType ac-type-name.
              rtab[FRAME-LINE] = recid(Account).
              IF order = 1 THEN FIND NEXT Account
              WHERE Account.Brand = lcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT Account USE-INDEX AccName
              WHERE Account.Brand = lcBrand no-lock no-error.
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
        ufk[1]= 36  ufk[2]= 30 ufk[3]= 927 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW Account.AccNum ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Account.AccNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Account.AccName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Account.AccName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND Account where recid(Account) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev Account
           WHERE Account.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev Account USE-INDEX AccName
           WHERE Account.Brand = lcBrand no-lock no-error.
           IF AVAILABLE Account THEN
              ASSIGN firstline = i memory = recid(Account).
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
           FIND Account where recid(Account) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev Account
           WHERE Account.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev Account USE-INDEX AccName
           WHERE Account.Brand = lcBrand no-lock no-error.
           IF NOT AVAILABLE Account THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              /* Checks names of Account type */
              ac-type-name = paiv(AccType).

              DISPLAY Account.Brand Account.AccNum Account.AccName
                     AccType ac-type-name.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(Account)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND Account where recid(Account) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT Account
           WHERE Account.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT Account USE-INDEX AccName
           WHERE Account.Brand = lcBrand no-lock no-error.
           IF NOT AVAILABLE Account THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              /* Checks names of Account type */
              ac-type-name = paiv(AccType).

              DISPLAY Account.Brand Account.AccNum Account.AccName
                      AccType ac-type-name.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(Account).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Account where recid(Account) = memory no-lock no-error.
        IF order = 1 THEN FIND prev Account
        WHERE Account.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND prev Account USE-INDEX AccName
        WHERE Account.Brand = lcBrand no-lock no-error.
        IF AVAILABLE Account THEN DO:
           memory = recid(Account).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev Account
              WHERE Account.Brand = lcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND prev Account USE-INDEX AccName
              WHERE Account.Brand = lcBrand no-lock no-error.
              IF AVAILABLE Account THEN memory = recid(Account).
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
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           memory = rtab[FRAME-DOWN].
           FIND Account where recid(Account) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* SEARCH 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       haku-ac-nr = 0.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME haku-f1.
       UPDATE lcBrand WHEN gcAllBrand 
              haku-ac-nr WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       IF haku-ac-nr <> 0 THEN DO:
          FIND FIRST Account where 
             Account.Brand = lcBrand  AND
             Account.AccNum >= haku-ac-nr 
             no-lock no-error.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* SEARCH 1 */

     /* SEARCH 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       haku-ac-name = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME haku-f2.
       UPDATE lcBrand WHEN gcAllBrand 
              haku-ac-name WITH FRAME haku-f2.
       HIDE FRAME haku-f2 no-pause.
       if haku-ac-name <> "" THEN DO:

          FIND FIRST Account USE-INDEX AccName WHERE 
             Account.Brand = lcBrand AND
             Account.AccName >= haku-ac-name 
             no-lock no-error.

          IF NOT fRecFound(2) THEN NEXT BROWSE.   

          NEXT LOOP.
       END.
     END. /* SEARCH 2 */

     if lookup(nap,"3,f3") > 0 
     THEN DO TRANS: /* memo */
        FIND account where recid(account) = rtab[frame-line(sel)]
        NO-LOCK NO-ERROR.
        RUN Mc/memo(INPUT 0,
                 INPUT "ACCOUNT",
                 INPUT STRING(account.AccNum),
                 INPUT "Account number").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* removal */
       {Syst/uright2.i}
       delline = FRAME-LINE.
       FIND Account where recid(Account) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
       Account.Brand
       Account.AccNum Account.AccName AccType ac-type-name.

       IF order = 1 THEN FIND NEXT Account
       WHERE Account.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT Account USE-INDEX AccName
       WHERE Account.Brand = lcBrand no-lock no-error.
       IF AVAILABLE Account THEN memory = recid(Account).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND Account where recid(Account) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev Account
          WHERE Account.Brand = lcBrand no-lock no-error.
          ELSE IF order = 2 THEN FIND prev Account USE-INDEX AccName
          WHERE Account.Brand = lcBrand no-lock no-error.
          IF AVAILABLE Account THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(Account).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND Account where recid(Account) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       Account.Brand
       Account.AccNum Account.AccName AccType ac-type-name.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhAccount).

           DELETE Account.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST Account
           WHERE Account.Brand = lcBrand) THEN DO:
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
       {Syst/uright2.i}
       /* change */
       FIND Account where recid(Account) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       /* Checks names of Account type */
       ac-type-name = paiv(AccType).

       DISPLAY 
          Account.Brand 
          Account.AccNum 
          ac-type-name
          Account.AccName
          Account.AccType.

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhAccount).

          UPDATE Account.AccName
                 Account.AccType
          WITH FRAME lis
          EDITING:

             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
             PAUSE 0.
               IF FRAME-FIELD = "AccType" THEN DO:
               /* Checks names of Account type */
               ac-type-name = paiv(INPUT FRAME lis AccType).
                  IF ac-type-name = "" THEN DO:
                    BELL.
                    MESSAGE "Unknown type !".
                    NEXT.
                  END.
                  DISP ac-type-name.
               END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */
          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhAccount).
       END.
       ELSE PAUSE.

       HIDE FRAME lis no-pause.
       /* Checks names of Account type */
       ac-type-name = paiv(AccType).

       DISPLAY Account.Brand Account.AccName
               AccType ac-type-name 
       WITH FRAME sel.
       xrecid = recid(Account).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST Account
       WHERE Account.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST Account USE-INDEX AccName
       WHERE Account.Brand = lcBrand no-lock no-error.
       ASSIGN memory = recid(Account) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST Account
       WHERE Account.Brand = lcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST Account USE-INDEX AccName
       WHERE Account.Brand = lcBrand no-lock no-error.
       ASSIGN memory = recid(Account) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

