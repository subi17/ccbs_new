/* ----------------------------------------------------------------------
  MODULE .......: MinConsumption
  TASK .........: Browse table MinConsumption
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 01.09.11
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/timestamp.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MinConsumption'}
{Func/luhnchecksum.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMinConsumption AS HANDLE NO-UNDO.
   lhMinConsumption = BUFFER MinConsumption:HANDLE.
   RUN StarEventInitialize(lhMinConsumption).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhMinConsumption).
   END.

END.

DEF INPUT PARAMETER iiMsSeq AS INT  NO-UNDO.
                     
DEF VAR liMsSeq       AS INT                 NO-UNDO.
DEF VAR xrecid        AS RECID               NO-UNDO  init ?.
DEF VAR FIRSTrow      AS INT                 NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                 NO-UNDO  init 3.
DEF VAR FrmDown       AS INT                 NO-UNDO  init 12.
DEF VAR order         AS INT                 NO-UNDO  init 1.
DEF VAR orders        AS CHAR                NO-UNDO.
DEF VAR maxOrder      AS INT                 NO-UNDO  init 1.
DEF VAR ufkey         AS LOG                 NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                 NO-UNDO  init 0.
DEF VAR pr-order      AS INT                 NO-UNDO.
DEF VAR Memory        AS RECID               NO-UNDO.
DEF VAR RowNo         AS INT                 NO-UNDO.
DEF VAR must-print    AS LOG                 NO-UNDO.
DEF VAR must-add      AS LOG                 NO-UNDO.
DEF VAR ac-hdr        AS CHAR                NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24     NO-UNDO.
DEF VAR i             AS INT                 NO-UNDO.
DEF VAR ok            AS log format "Yes/No" NO-UNDO.
DEF VAR lcExtInvID    AS CHAR                NO-UNDO.


form
    MinConsumption.MsSeq     
    MinConsumption.FromDate
    MinConsumption.ToDate
    lcExtInvID      FORMAT "X(12)" COLUMN-LABEL "Invoice"
    MinConsumption.Amount COLUMN-LABEL "Amount"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " MINIMUM CONSUMPTION " FRAME sel.

form
    MinConsumption.MsSeq COLON 20
    MinConsumption.FromDate COLON 20
    MinConsumption.ToDate COLON 20
    lcExtInvID  COLON 20 FORMAT "X(12)" LABEL "Invoice"
    MinConsumption.Amount   COLON 20
WITH  OVERLAY ROW 4 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form 
    "Subscription ID:" liMsSeq FORMAT ">>>>>>>9"
    HELP "Enter subscription ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Subscription "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE MinConsumption THEN ASSIGN
   Memory       = recid(MinConsumption)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.
    
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MinConsumption WHERE recid(MinConsumption) = Memory 
          NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MinConsumption THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MinConsumption).
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
        ufk    = 0
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MinConsumption.MsSeq ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MinConsumption.MsSeq WITH FRAME sel.
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
        FIND MinConsumption WHERE recid(MinConsumption) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MinConsumption THEN
              ASSIGN FIRSTrow = i Memory = recid(MinConsumption).
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
           IF NOT AVAILABLE MinConsumption THEN DO:
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
                rtab[1] = recid(MinConsumption)
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
           IF NOT AVAILABLE MinConsumption THEN DO:
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
              rtab[FRAME-DOWN] = recid(MinConsumption).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MinConsumption WHERE recid(MinConsumption) = Memory
            NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MinConsumption THEN DO:
           Memory = recid(MinConsumption).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MinConsumption THEN Memory = recid(MinConsumption).
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
           FIND MinConsumption WHERE recid(MinConsumption) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       UPDATE liMsSeq WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF liMsSeq > 0 THEN DO:
       
          FIND FIRST MinConsumption WHERE 
                     MinConsumption.MsSeq >= liMsSeq NO-LOCK NO-ERROR.

          IF NOT AVAILABLE MinConsumption THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(MinConsumption) 
                 must-print = TRUE
                 order      = 1.
          NEXT LOOP.

       END.
     END. /* Search-1 */


     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0  
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       COLOR DISPLAY VALUE(ctc)
       MinConsumption.MsSeq MinConsumption.FromDate
       MinConsumption.ToDate MinConsumption.Amount.

       RUN local-find-NEXT.
       IF AVAILABLE MinConsumption THEN Memory = recid(MinConsumption).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE MinConsumption THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(MinConsumption).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       MinConsumption.MsSeq MinConsumption.FromDate
       MinConsumption.ToDate MinConsumption.Amount.

       IF ok THEN DO:

           FIND CURRENT MinConsumption EXCLUSIVE-LOCK.
           
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMinConsumption).

           DELETE MinConsumption.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE MinConsumption THEN DO:
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
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMinConsumption).

       ASSIGN ac-hdr = " TERMINAL " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMinConsumption).

       RUN local-disp-row.
       xrecid = recid(MinConsumption).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MinConsumption) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MinConsumption) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

fCleanEventObjects().


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND MinConsumption WHERE recid(MinConsumption) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MinConsumption WHERE recid(MinConsumption) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN 
       FIND FIRST MinConsumption WHERE MinConsumption.MsSeq = iiMsSeq 
          NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN 
       FIND LAST MinConsumption WHERE MinConsumption.MsSeq = iiMsSeq 
          NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-find-NEXT:

       FIND NEXT MinConsumption WHERE MinConsumption.MsSeq = iiMsSeq 
          NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
 
       FIND PREV MinConsumption WHERE MinConsumption.MsSeq = iiMsSeq 
          NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
          MinConsumption.MsSeq     
          MinConsumption.FromDate
          MinConsumption.ToDate
          lcExtInvID
          MinConsumption.Amount
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   lcExtInvId = "".
   FIND FIRST Invoice WHERE Invoice.InvNum = MinConsumption.InvNum 
      NO-LOCK NO-ERROR.
   IF AVAILABLE Invoice THEN lcExtInvId = Invoice.ExtInvID.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      DISP 
         MinConsumption.MsSeq     
         MinConsumption.FromDate
         MinConsumption.ToDate 
         lcExtInvID
         MinConsumption.Amount
      WITH FRAME lis.
      
      IF NOT NEW MinConsumption THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.
      
      LEAVE. 
   END.

END PROCEDURE.



