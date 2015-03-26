/* ----------------------------------------------------------------------
  MODULE .......: TOKEN.P
  TASK .........: Update Tokens
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 18.02.03
  CHANGED ......: 11.03.03 tk tokens
  VERSION ......: M15
  ---------------------------------------------------------------------- */

{commali.i}
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'token'}
{fuserright.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhtoken AS HANDLE NO-UNDO.
   lhtoken = BUFFER token:HANDLE.
   RUN StarEventInitialize(lhtoken).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhtoken).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR tokencode      LIKE token.tokencode        NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR crundo       AS LOG                    NO-UNDO INIT FALSE.
DEF VAR llAdminUser  AS LOG                    NO-UNDO INIT FALSE.

form
    token.tokencode  /* COLUMN-LABEL FORMAT */
    token.tokenname
    Token.AdminToken
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " TOKENS "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    token.tokencode     /* LABEL FORMAT */
    token.tokenname
    Token.AdminToken
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.

form /* seek Token by Code */
    tokencode
    help "Enter Token Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".

llAdminUser = fIsAdminUser(katun).

FIND FIRST token
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE token THEN ASSIGN
   memory       = recid(token)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No tokens available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a token  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN ufcolor.

ADD-ROW:
      DO WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN ufkey.
        DO TRANSAction:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR token.tokencode
           VALIDATE
              (token.tokencode = "" OR                        
              NOT CAN-FIND(token using  token.tokencode),
              "Token " + INPUT token.tokencode +
              " already exists !").
           IF INPUT token.tokencode = "" THEN LEAVE ADD-ROW.
           CREATE token.
           ASSIGN
           token.tokencode  = INPUT FRAME lis token.tokencode
           Token.AdminToken = FALSE.

           RUN local-update-record.

           IF crundo = TRUE THEN DO:
              crundo = FALSE.
              UNDO add-row, LEAVE add-row.
           END.

           ASSIGN
           memory = recid(token)
           xrecid = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhtoken).

      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST token
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE token THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND token WHERE recid(token) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE token THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(token).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 no-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35 ufk[2]= 0 ufk[3]= 0 ufk[4]= 2203
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW token.tokencode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) token.tokencode WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND token WHERE recid(token) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE token THEN
              ASSIGN FIRSTrow = i memory = recid(token).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE token THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(token)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE token THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(token).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND token WHERE recid(token) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE token THEN DO:
           memory = recid(token).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE token THEN memory = recid(token).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND token WHERE recid(token) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       tokencode = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE tokencode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF tokencode <> "" THEN DO:
          FIND FIRST token WHERE token.tokencode >= tokencode
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE token THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some token/vc-code was found */
          ASSIGN order = 1 memory = recid(token) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN tokentab.p(token.tokencode).
        ufkey = true.
        next loop.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST TableToken WHERE 
                         TableToken.TokenCode = Token.TokenCode)
       THEN DO:
          MESSAGE "Token has been assigned to tables. Deletion not allowed"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
       
       IF CAN-FIND(FIRST MenuTree WHERE 
                         MenuTree.TokenCode = Token.TokenCode)
       THEN DO:
          MESSAGE "Token has been assigned to menus. Deletion not allowed"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.

       IF Token.AdminToken AND NOT llAdminUser THEN DO:
          MESSAGE "This is an admin level token. Deletion not allowed"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          token.tokencode 
          token.tokenname.

       RUN local-find-NEXT.
       IF AVAILABLE token THEN memory = recid(token).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE token THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(token).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          token.tokencode 
          token.tokenname.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhtoken).

           DELETE token.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST token
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     DO WITH FRAME lis TRANSAction:
       {uright2.i} 
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       cfc = "lis". RUN ufcolor.
       CLEAR FRAME lis NO-PAUSE.
       DISPLAY token.tokencode.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhtoken).

       RUN local-update-record.

       IF crundo = TRUE THEN DO:
          crundo = FALSE.
          HIDE FRAME lis NO-PAUSE.
          UNDO, Leave.

       END.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhtoken).

       HIDE FRAME lis NO-PAUSE.
       RUN local-disp-row.
       xrecid = recid(token).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(token) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(token) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    IF exlock THEN
      FIND token WHERE recid(token) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND token WHERE recid(token) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST token
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST token
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT token
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev token
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       DISPLAY
       token.tokencode
       token.tokenname
       Token.AdminToken
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-update-record:
   
   RUN local-find-others.
   
   DISP
   Token.TokenCode
   WITH FRAME lis.
   
   UPDATE
       token.tokenname
       Token.AdminToken WHEN llAdminUser

   WITH FRAME lis EDITING  :
          READKEY.

          IF LOOKUP(KEYLABEL(LASTKEY),"f4") > 0 THEN DO:
             crundo = TRUE.
             leave.
          END.

          IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
             PAUSE 0.

          END.
          APPLY LASTKEY.
       END. /* EDITING */

END PROCEDURE.

