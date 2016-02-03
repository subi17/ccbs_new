/* ----------------------------------------------------------------------
  MODULE .......: barringconf.p
  TASK .........: barringconf
  APPLICATION ..: nn
  AUTHOR .......: ilkkasav
  CREATED ......: 11-05-2015
  CHANGED ......: 
  Version ......: 
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'BarringConf'}



IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhBConf AS HANDLE NO-UNDO.
   lhBConf = BUFFER BarringConf:HANDLE.
   RUN StarEventInitialize(lhBConf).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhBConf).
   END.
END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Bcode  LIKE BarringConf.BarringGroup  NO-UNDO.
DEF VAR CoName  LIKE BarringConf.BarringCode NO-UNDO.
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
    BarringConf.BarringGroup      /* COLUMN-LABEL FORMAT */
    BarringConf.BarringCode     /* COLUMN-LABEL FORMAT */
    BarringConf.BarringStatus     /* COLUMN-LABEL FORMAT */

             /* COLUMN-LABEL FORMAT */
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Barrings "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    BarringConf.BarringGroup   /* LABEL FORMAT */
    BarringConf.BarringCode    /* LABEL FORMAT */
    BarringConf.OldCode
    BarringConf.BarringStatus  /* LABEL FORMAT */
    BarringConf.NWActParam
    BarringConf.NWDeactPAram
    BarringConf.NWComponent
    BarringConf.Mask
    BarringConf.AllowedAppIDs
    BarringConf.AllowedPaymentType
    BarringConf.UIPriority
    BarringConf.IFSPriority FORMAT ">>9"

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* By barring code */
    BCode
    HELP "Barring code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND GROUP "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* By Name */
    CoName
    HELP "Enter Barring Name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By Name,By 3, By 4".


FIND FIRST BarringConf
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE BarringConf THEN ASSIGN
   Memory       = recid(BarringConf)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No barrings available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.
LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR BarringConf.BarringGroup
           VALIDATE
              (BarringConf.BarringGroup NOT ENTERED OR
              NOT CAN-FIND(BarringConf using  BarringConf.BarringGroup),
              "Barring group " + string(INPUT BarringConf.BarringGroup) +
              " already exists !").
           IF INPUT FRAME lis BarringConf.BarringGroup NOT ENTERED THEN 
           LEAVE add-row.
           CREATE BarringConf.
           ASSIGN
           BarringConf.BarringGroup = INPUT FRAME lis BarringConf.BarringGroup.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBConf).

           ASSIGN
           Memory = recid(BarringConf)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST BarringConf
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BarringConf THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND BarringConf WHERE recid(BarringConf) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE BarringConf THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(BarringConf).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 927 ufk[4]= 814
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW BarringConf.BarringGroup ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) BarringConf.BarringGroup WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW BarringConf.BarringCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) BarringConf.BarringCode WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND BarringConf WHERE recid(BarringConf) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE BarringConf THEN
              ASSIGN FIRSTrow = i Memory = recid(BarringConf).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE BarringConf THEN DO:
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
                rtab[1] = recid(BarringConf)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE BarringConf THEN DO:
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
              rtab[FRAME-DOWN] = recid(BarringConf).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND BarringConf WHERE recid(BarringConf) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE BarringConf THEN DO:
           Memory = recid(BarringConf).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE BarringConf THEN Memory = recid(BarringConf).
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
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND BarringConf WHERE recid(BarringConf) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET BarringConf WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF BCode ENTERED THEN DO:
          FIND FIRST BarringConf WHERE BarringConf.BarringGroup >= BCode
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE BarringConf THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some BarringConf/BarringConf was found */
          ASSIGN order = 1 Memory = recid(BarringConf) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       SET CoName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CoName ENTERED THEN DO:
          FIND FIRST BarringConf WHERE BarringConf.BarringCode >= CoName
          USE-INDEX BarringCode /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE BarringConf THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some BarringConf/CoName was found */
          ASSIGN order = 2 Memory = recid(BarringConf) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     if lookup(nap,"3,f3") > 0 
     THEN DO TRANS: /* memo */
        FIND BarringConf where recid(BarringConf) = rtab[frame-line(sel)]
        NO-LOCK NO-ERROR.
        RUN memo(INPUT 0,
                 INPUT "BarringConf",
                 INPUT BarringConf.BarringGroup,
                 INPUT "BarringConf").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     /* translations */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  
         FIND BarringConf WHERE RECID(BarringConf) = rtab[FRAME-LINE] NO-LOCK.
         RUN invlang(5,STRING(BarringConf.BarringCode)).
         
         ufkey = TRUE.
         NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}.
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       BarringConf.BarringGroup BarringConf.BarringCode .

       RUN local-find-NEXT.
       IF AVAILABLE BarringConf THEN Memory = recid(BarringConf).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE BarringConf THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(BarringConf).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       BarringConf.BarringGroup BarringConf.BarringCode .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhBConf).

           DELETE BarringConf.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST BarringConf
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

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       {Syst/uright2.i}
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBConf).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY BarringConf.BarringGroup.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBConf).

       RUN local-disp-row.
       xrecid = recid(BarringConf).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(BarringConf) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(BarringConf) must-print = TRUE.
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
      FIND BarringConf WHERE recid(BarringConf) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND BarringConf WHERE recid(BarringConf) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST BarringConf
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST BarringConf USE-INDEX BarringCode
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST BarringConf
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST BarringConf USE-INDEX BarringCode
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT BarringConf
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT BarringConf USE-INDEX BarringCode
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV BarringConf
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV BarringConf USE-INDEX BarringCode
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       BarringConf.BarringGroup
       BarringConf.BarringCode
       BarringConf.BarringStatus

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          BarringConf.BarringCode
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:

         UPDATE
             BarringConf.BarringGroup
             BarringConf.BarringCode
             BarringConf.OldCode
             BarringConf.BarringStatus
             BarringConf.Mask
             BarringConf.NWActParam
             BarringConf.NWDeactPAram
             BarringConf.NWComponent
             BarringConf.AllowedAppIDs
             BarringConf.AllowedPaymentType
             BarringConf.UIPriority
             BarringConf.IFSPriority


         WITH FRAME lis.

         /*IF BarringConf.BarringStatus NE "" /*AND
            NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                 "BarringConf",
                                 "FraudGroup",
                                 BarringConf.BarringStatus)*/
         THEN DO:
            MESSAGE "Unknown group"
            VIEW-AS ALERT-BOX ERROR.
            UNDO, NEXT.
         END.*/
      END.
      ELSE PAUSE.

      LEAVE.
   END.
END PROCEDURE.




