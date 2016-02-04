/* ----------------------------------------------------------------------
  MODULE .......: PreselErr.P
  TASK .........: Update Mobile Rating Error Codes
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 18.11.99
  CHANGED ......: 01.11.02 jr Eventlog                            
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}  
{Syst/eventval.i} 

def /* new */ shared var siirto AS char.

DEF VAR ReturnCode  like PreselErr.PSError  NO-UNDO.
DEF VAR PSEName  like PreselErr.PSEName NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 2.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR new_preselerr AS LOG                   NO-UNDO INIT FALSE.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPreselErr AS HANDLE NO-UNDO.
   lhPreselErr = BUFFER PreselErr:HANDLE.
   RUN StarEventInitialize(lhPreselErr).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhPreselErr).
   END.
END.

form
    PreselErr.PSError      /* column-label format */
    PreselErr.PSEName      /* column-label format */

WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Preselection Transaction Error Codes "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    PreselErr.PSError     /* label format */
    PreselErr.PSEName     /* label format */

WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek PreselErr  by  ReturnCode */
    ReturnCode
    HELP "Enter Code of pserr"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek PreselErr  by PSEName */
    PSEName
    HELP "Enter BankOffice of pserr"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND BankOffice "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.



cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By BankOffice,By 3, By 4".


FIND FIRST PreselErr
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE PreselErr THEN ASSIGN
   memory       = recid(PreselErr)
   must-print   = true
   must-add     = false.
ELSE ASSIGN
   memory       = ?
   must-print   = false
   must-add     = true.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a PreselErr  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR PreselErr.PSError
           validate
              (PreselErr.PSError = 0 or
              NOT CAN-FIND(PreselErr using  PreselErr.PSError),
              "Error Code " + string(INPUT PreselErr.PSError) +
              " already exists !").
           IF INPUT FRAME lis PreselErr.PSError = 0 THEN 
           LEAVE add-row.
           create PreselErr.
           ASSIGN
           PreselErr.PSError = INPUT FRAME lis PreselErr.PSError.
           new_preselerr = TRUE.
           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           memory = recid(PreselErr)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST PreselErr
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE PreselErr THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND PreselErr WHERE recid(PreselErr) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PreselErr THEN DO:
              RUN local-dipe-row.
              rtab[FRAME-line] = recid(PreselErr).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, retuRN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35 ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
        {Syst/uright1.i '"4,5,6"'}
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row PreselErr.PSError ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PreselErr.PSError WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row PreselErr.PSEName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PreselErr.PSEName WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        choose row PreselErr.?? ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PreselErr.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        choose row PreselErr.??  ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PreselErr.? WITH FRAME sel.
      END.
*/
      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND PreselErr WHERE recid(PreselErr) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE PreselErr THEN
              ASSIGN FIRSTrow = i memory = recid(PreselErr).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE PreselErr THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-dipe-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(PreselErr)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE PreselErr THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-dipe-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(PreselErr).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND PreselErr WHERE recid(PreselErr) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PreselErr THEN DO:
           memory = recid(PreselErr).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE PreselErr THEN memory = recid(PreselErr).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND PreselErr WHERE recid(PreselErr) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME f1.
       SET ReturnCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ReturnCode ENTERED THEN DO:
          FIND FIRST PreselErr WHERE PreselErr.PSError >= ReturnCode
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE PreselErr THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some pserr/pe-code was found */
          ASSIGN order = 1 memory = recid(PreselErr) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME f2.
       SET PSEName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF PSEName ENTERED THEN DO:
          FIND FIRST PreselErr WHERE PreselErr.PSEName >= PSEName
          USE-INDEX PSEName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE PreselErr THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some pserr/pe-name was found */
          ASSIGN order = 2 memory = recid(PreselErr) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */



     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSAction:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-line.
       RUN local-find-this (false).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       PreselErr.PSError PreselErr.PSEName .

       RUN local-find-NEXT.
       IF AVAILABLE PreselErr THEN memory = recid(PreselErr).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE PreselErr THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(PreselErr).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       PreselErr.PSError PreselErr.PSEName .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPreselErr).
           DELETE PreselErr.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST PreselErr
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       {Syst/uright2.i}
       /* change */
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PreselErr.PSError.
       new_preselerr = FALSE.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPreselErr).
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-dipe-row.
       xrecid = recid(PreselErr).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(PreselErr) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(PreselErr) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find PreselErr WHERE recid(PreselErr) = rtab[FRAME-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find PreselErr WHERE recid(PreselErr) = rtab[FRAME-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST PreselErr
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST PreselErr USE-INDEX PSEName
       /* srule */ NO-LOCK NO-ERROR.
    /* ELSE IF order = 3 THEN FIND FIRST PreselErr USE-INDEX index-3
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST PreselErr USE-INDEX index-4
       /* srule */ NO-LOCK NO-ERROR.   */
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST PreselErr
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST PreselErr USE-INDEX PSEName
       /* srule */ NO-LOCK NO-ERROR.
    /* ELSE IF order = 3 THEN FIND LAST PreselErr USE-INDEX index-3
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND LAST PreselErr USE-INDEX index-4
       /* srule */ NO-LOCK NO-ERROR.   */
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT PreselErr
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT PreselErr USE-INDEX PSEName
       /* srule */ NO-LOCK NO-ERROR.
    /* ELSE IF order = 3 THEN FIND NEXT PreselErr USE-INDEX index-3
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND NEXT PreselErr USE-INDEX index-4
       /* srule */ NO-LOCK NO-ERROR.   */
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV PreselErr
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV PreselErr USE-INDEX PSEName
       /* srule */ NO-LOCK NO-ERROR.
    /* ELSE IF order = 3 THEN FIND PREV PreselErr USE-INDEX index-3
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND PREV PreselErr USE-INDEX index-4
       /* srule */ NO-LOCK NO-ERROR.   */
END PROCEDURE.

PROCEDURE local-dipe-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       PreselErr.PSError
       PreselErr.PSEName

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          PreselErr.PSEName

      WITH FRAME lis.
   /* EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "<field>" THEN DO:
                   FIND <table> WHERE <table>.<field> =
                   INPUT FRAME lis PreselErr.<field> NO-LOCK NO-ERROR.
                   IF NOT AVAIL <table> THEN DO:
                      BELL.
                      MESSAGE "Unknown <table> !".
                      NEXT.
                   END.
                   DISP <table>.<field2>.
                END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */
*/
      IF NOT new_preselerr AND llDoEvent 
      THEN RUN StarEventMakeModifyEvent(lhPreselErr).

      IF new_preselerr AND llDoEvent 
      THEN RUN StarEventMakeCreateEvent(lhPreselErr).

      new_preselerr = FALSE.
      LEAVE.
   END.
END PROCEDURE.
