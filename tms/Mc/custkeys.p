/* ----------------------------------------------------------------------
  MODULE .......: custkeys.p
  TASK .........: Customer related userkeys
  APPLICATION ..: TMS
  AUTHOR .......: TK
  CREATED ......: 07.04.04
  CHANGED ......: 
  Version ......: MTV3
  ---------------------------------------------------------------------- */

{Syst/testpaa.i}
{Syst/eventval.i}

def var /*input parameter*/ ipCustnum like mobsub.msseq no-undo.

find customer 10001 no-lock.
ipCustnum = customer.custnum.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhuserkeys AS HANDLE NO-UNDO.
   lhuserkeys = BUFFER userkeys:HANDLE.
   RUN StarEventInitialize(lhuserkeys).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhuserkeys).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR usercode      LIKE userkeys.usercode        NO-UNDO format "X(16)".
DEF VAR keytype      LIKE userkeys.keytype        NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

form
    userkeys.usercode   FORMAT "X(18)" 
    userkeys.keytype    
    userkeys.key
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
    " USER KEYS "
    + string(TODAY,"99-99-99") + " "
    FRAME sel.

form
    userkeys.usercode  format "x(18)"
    userkeys.keytype  
    userkeys.key
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  userkeys */
    usercode
    HELP "Enter user code"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND CODE "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  keytype */
    keytype
    HELP "Enter key type"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Name "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.


Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".

find customer no-lock where customer.custnum = ipCustnum.


RUN local-find-first.
IF AVAILABLE userkeys THEN ASSIGN
   Memory       = recid(userkeys)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = TRUE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a userkeys  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR userkeys.usercode
           VALIDATE
              (userkeys.usercode NOT ENTERED OR
              NOT CAN-FIND(userkeys using  userkeys.usercode),
              "Billing RepType " + string(INPUT userkeys.usercode) +
              " already exists !").
           IF INPUT FRAME lis userkeys.usercode = "" THEN 
           LEAVE add-row.
           CREATE userkeys.
           ASSIGN
           userkeys.usercode = INPUT FRAME lis userkeys.usercode.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhuserkeys).

           ASSIGN
           Memory = recid(userkeys)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST userkeys
      WHERE userkeys.key = string(customer.custnum) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE userkeys THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND userkeys WHERE recid(userkeys) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE userkeys THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(userkeys).
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
        PAUSE 0 NO-MESSAGE.

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
        Syst.Var:ufk[1]= 35  Syst.Var:ufk[2]= 30 Syst.Var:ufk[3]= 0  Syst.Var:ufk[4]= 0
        Syst.Var:ufk[5]= 5  Syst.Var:ufk[6]= 4 Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW userkeys.usercode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) userkeys.usercode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW userkeys.keytype {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) userkeys.keytype WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = keylabel(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND userkeys WHERE recid(userkeys) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE userkeys THEN
              ASSIGN FIRSTrow = i Memory = recid(userkeys).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE userkeys THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(userkeys)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE userkeys THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(userkeys).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND userkeys WHERE recid(userkeys) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE userkeys THEN DO:
           Memory = recid(userkeys).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE userkeys THEN Memory = recid(userkeys).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */

     /* NEXT page */
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND userkeys WHERE recid(userkeys) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET usercode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF usercode ENTERED THEN DO:
          FIND FIRST userkeys WHERE userkeys.usercode >= usercode
          AND userkeys.usercode = string(customer.custnum) NO-LOCK NO-ERROR.
          IF NOT AVAILABLE userkeys THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some userkeys/userkeys was found */
          ASSIGN order = 1 Memory = recid(userkeys) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       SET keytype WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF keytype ENTERED THEN DO:
          FIND FIRST userkeys WHERE userkeys.keytype >= keytype
          AND userkeys.usercode = string(customer.custnum) USE-INDEX keytype NO-LOCK NO-ERROR.
          IF NOT AVAILABLE userkeys THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some userkeys/keytype was found */
          ASSIGN order = 2 Memory = recid(userkeys) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */


     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       userkeys.usercode userkeys.keytype .

       RUN local-find-NEXT.
       IF AVAILABLE userkeys THEN Memory = recid(userkeys).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE userkeys THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(userkeys).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       userkeys.usercode userkeys.keytype .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhuserkeys).

           DELETE userkeys.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST userkeys
           WHERE userkeys.usercode = string(customer.custnum)) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhuserkeys).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE Syst.Var:ehto = 9. RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY userkeys.usercode.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhuserkeys).

       RUN local-disp-row.
       xrecid = recid(userkeys).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(userkeys) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(userkeys) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND userkeys WHERE recid(userkeys) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND userkeys WHERE recid(userkeys) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST userkeys
       WHERE userkeys.usercode = string(customer.custnum) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST userkeys USE-INDEX keytype
       WHERE userkeys.usercode = string(customer.custnum) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST userkeys
       WHERE userkeys.usercode = string(customer.custnum) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST userkeys USE-INDEX keytype
       WHERE userkeys.usercode = string(customer.custnum) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT userkeys 
       WHERE userkeys.usercode = string(customer.custnum) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT userkeys USE-INDEX keytype
       WHERE userkeys.usercode = string(customer.custnum) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV userkeys
       WHERE userkeys.usercode = string(customer.custnum) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV userkeys USE-INDEX keytype
       WHERE userkeys.usercode = string(customer.custnum) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       userkeys.usercode 
       userkeys.keytype
       userkeys.key
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          userkeys.keytype
          userkeys.key

      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

