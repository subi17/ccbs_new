/* ----------------------------------------------------------------------
  MODULE .......: BillType
  TASK .........: UPDATEs table BillType
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 21.05.02/tk Event logging added
                  28.02.03 tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'billtype'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhBillType AS HANDLE NO-UNDO.
   lhBillType = BUFFER BillType:HANDLE.
   RUN StarEventInitialize(lhBillType).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhBillType).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR BillType      LIKE BillType.BillType        NO-UNDO.
DEF VAR BTName      LIKE BillType.BTName        NO-UNDO.
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
    BillType.BillType     /* COLUMN-LABEL FORMAT */
    BillType.BTName     /* COLUMN-LABEL FORMAT */

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.CUICommon:cfc)   
    TITLE COLOR VALUE(Syst.CUICommon:ctc) " " + Syst.CUICommon:ynimi +
    "  BILLING ITEMS MENU  "
    + string(TODAY,"99-99-99") + " "
    FRAME sel.

form
    BillType.BillType     /* LABEL FORMAT */
    BillType.BTName    /* LABEL FORMAT */
            /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  BillType */
    BillType
    HELP "Enter Code of Billing RepType "
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND CODE "
    COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  BTName */
    BTName
    HELP "Enter Name of the Billing RepType"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND Name "
    COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f2.

form
    BillType.Memo

    WITH OVERLAY ROW 3 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc)
    " Memo: " + BillType.BTName + " " WITH NO-LABELS 1 columns
    FRAME f4.


Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST BillType
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE BillType THEN ASSIGN
   Memory       = recid(BillType)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No billing items available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a BillType  */
      ASSIGN Syst.CUICommon:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR BillType.BillType
           VALIDATE
              (BillType.BillType NOT ENTERED OR
              NOT CAN-FIND(BillType using  BillType.BillType),
              "Billing RepType " + string(INPUT BillType.BillType) +
              " already exists !").
           IF INPUT FRAME lis BillType.BillType = "" THEN 
           LEAVE add-row.
           CREATE BillType.
           ASSIGN
           BillType.BillType = INPUT FRAME lis BillType.BillType.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBillType).

           ASSIGN
           Memory = recid(BillType)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST BillType
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BillType THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND BillType WHERE recid(BillType) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE BillType THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(BillType).
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
        Syst.CUICommon:ufk[1]= 35  Syst.CUICommon:ufk[2]= 30 Syst.CUICommon:ufk[3]= 0  Syst.CUICommon:ufk[4]= 927
        Syst.CUICommon:ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        Syst.CUICommon:ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        Syst.CUICommon:ufk[7]= 0 Syst.CUICommon:ufk[8]= 8 Syst.CUICommon:ufk[9]= 1
        Syst.CUICommon:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW BillType.BillType {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.CUICommon:ccc) BillType.BillType WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW BillType.BTName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.CUICommon:ccc) BillType.BTName WITH FRAME sel.
      END.

      Syst.CUICommon:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.CUICommon:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(Syst.CUICommon:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.CUICommon:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND BillType WHERE recid(BillType) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE BillType THEN
              ASSIGN FIRSTrow = i Memory = recid(BillType).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.CUICommon:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE BillType THEN DO:
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
                rtab[1] = recid(BillType)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.CUICommon:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE BillType THEN DO:
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
              rtab[FRAME-DOWN] = recid(BillType).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.CUICommon:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND BillType WHERE recid(BillType) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE BillType THEN DO:
           Memory = recid(BillType).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE BillType THEN Memory = recid(BillType).
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
     ELSE IF LOOKUP(Syst.CUICommon:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND BillType WHERE recid(BillType) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.CUICommon:nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET BillType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF BillType ENTERED THEN DO:
          FIND FIRST BillType WHERE BillType.BillType >= BillType
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE BillType THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some BillType/BillType was found */
          ASSIGN order = 1 Memory = recid(BillType) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(Syst.CUICommon:nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       SET BTName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF BTName ENTERED THEN DO:
          FIND FIRST BillType WHERE BillType.BTName >= BTName
          USE-INDEX BTName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE BillType THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some BillType/BTName was found */
          ASSIGN order = 2 Memory = recid(BillType) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* UPDATE memo */
     ELSE IF LOOKUP(Syst.CUICommon:nap,"4,f4") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP:
        Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
        Syst.CUICommon:ehto = 9. 
        RUN Syst/ufkey.p. ufkey = TRUE.
        RUN local-find-this(TRUE).
        IF lcRight = "RW" THEN 
           UPDATE BillType.Memo WITH FRAME f4.
        ELSE DO:
           DISP   BillType.Memo WITH FRAME f4.
           PAUSE.
        END.
        HIDE FRAME f4 NO-PAUSE.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.CUICommon:ctc)
       BillType.BillType BillType.BTName .

       RUN local-find-NEXT.
       IF AVAILABLE BillType THEN Memory = recid(BillType).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE BillType THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(BillType).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.CUICommon:ccc)
       BillType.BillType BillType.BTName .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhBillType).

           DELETE BillType.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST BillType
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.CUICommon:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBillType).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.
       Syst.CUICommon:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY BillType.BillType.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBillType).

       RUN local-disp-row.
       xrecid = recid(BillType).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(BillType) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(BillType) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND BillType WHERE recid(BillType) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND BillType WHERE recid(BillType) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST BillType
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST BillType USE-INDEX BTName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST BillType
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST BillType USE-INDEX BTName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT BillType
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT BillType USE-INDEX BTName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV BillType
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV BillType USE-INDEX BTName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       BillType.BillType 
       BillType.BTName

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          BillType.BTName
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         UPDATE
            BillType.BTName
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

