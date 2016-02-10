/* ----------------------------------------------------------------------
  MODULE .......: Counter
  TASK .........: UPDATEs table Counter
  APPLICATION ..: TMS
  AUTHOR .......: rafaeldv
  CREATED ......: 11.11.09
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/timestamp.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Counter'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCounter AS HANDLE NO-UNDO.
   lhCounter = BUFFER Counter:HANDLE.
   RUN StarEventInitialize(lhCounter).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhCounter).
   END.

END.

DEF INPUT PARAMETER icHostTable   AS CHAR  NO-UNDO.
DEF INPUT PARAMETER icKeyValue    AS CHAR  NO-UNDO.

                     
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
DEF VAR lcCTypeName      AS CHAR                NO-UNDO.
DEF VAR lcHeader      AS CHAR                NO-UNDO init "General Counters".
DEF VAR liCType       AS INT                 NO-UNDO. 

form
    Counter.CounterType 
    lcCTypeName    FORMAT "X(10)" COLUMN-LABEL "Type"
    Counter.CounterAmt   
    Counter.BeginStamp   
    Counter.EndStamp 

WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) lcHeader FRAME sel.

form
    Counter.Brand         COLON 22 FORMAT "X(2)" SKIP
    Counter.CounterSeq    COLON 22 SKIP
    Counter.HostTable     COLON 22 FORMAT "X(10)" SKIP
    Counter.KeyValue      COLON 22 FORMAT "X(15)" SKIP
    Counter.CounterType   COLON 22 
    lcCTypeName FORMAT "X(20)" NO-LABEL SKIP
    Counter.CounterAmt   COLON 22 SKIP
    Counter.BeginStamp   COLON 22 SKIP
    Counter.EndStamp     COLON 22 
WITH  OVERLAY ROW 4 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form 
    "Counter Type:" liCType FORMAT ">>>>>>>9"
    HELP "Enter Counter Type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Counter Type "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FUNCTION fCTName RETURNS LOGIC
   ( INPUT piCType AS INT):
   
   lcCTypeName = "".
   FIND TMSCodes  WHERE 
        TMSCodes.TableName = "Counter" AND
        TMSCodes.FieldName = "CounterType" AND
        TMSCodes.CodeValue = STRING(piCType) NO-LOCK NO-ERROR.
   IF AVAILABLE TMSCodes THEN lcCTypeName = TMSCodes.CodeName .

END FUNCTION.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-find-first.

IF AVAILABLE Counter THEN ASSIGN
   Memory       = recid(Counter)
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
        FIND Counter WHERE recid(Counter) = Memory 
          NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Counter THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Counter).
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
        ufk[1] = 759
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Counter.CounterType ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Counter.CounterType WITH FRAME sel.
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
        FIND Counter WHERE recid(Counter) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Counter THEN
              ASSIGN FIRSTrow = i Memory = recid(Counter).
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
           IF NOT AVAILABLE Counter THEN DO:
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
                rtab[1] = recid(Counter)
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
           IF NOT AVAILABLE Counter THEN DO:
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
              rtab[FRAME-DOWN] = recid(Counter).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Counter WHERE recid(Counter) = Memory
            NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Counter THEN DO:
           Memory = recid(Counter).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Counter THEN Memory = recid(Counter).
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
           FIND Counter WHERE recid(Counter) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       UPDATE liCType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF liCType > 0 THEN DO:
       
           FIND FIRST Counter NO-LOCK WHERE 
                      Counter.Brand = gcBrand AND 
                      Counter.HostTable = icHostTable AND
                      Counter.KeyValue  = icKeyValue  AND
                      Counter.CounterType = liCType USE-INDEX HostTable NO-ERROR.         

          IF NOT AVAILABLE Counter THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(Counter) 
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
       Counter.CounterType Counter.CounterAmt
       Counter.BeginStamp Counter.EndStamp .

       RUN local-find-NEXT.
       IF AVAILABLE Counter THEN Memory = recid(Counter).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE Counter THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(Counter).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ctc)
       Counter.CounterType Counter.CounterAmt
       Counter.BeginStamp Counter.EndStamp .

       IF ok THEN DO:

           FIND CURRENT Counter EXCLUSIVE-LOCK.
           
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCounter).

           DELETE Counter.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE Counter THEN DO:
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
           
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCounter).

       ASSIGN ac-hdr = " COUNTER " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCounter).

       RUN local-disp-row.
       xrecid = recid(Counter).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Counter) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Counter) must-print = TRUE.
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
      FIND Counter WHERE recid(Counter) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Counter WHERE recid(Counter) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
 FIND FIRST Counter NO-LOCK WHERE 
            Counter.Brand = gcBrand AND 
            Counter.HostTable = icHostTable AND
            Counter.KeyValue  = icKeyValue USE-INDEX HostTable NO-ERROR.         
END PROCEDURE.

PROCEDURE local-find-LAST:
  FIND LAST Counter NO-LOCK WHERE 
            Counter.Brand = gcBrand AND 
            Counter.HostTable = icHostTable AND
            Counter.KeyValue  = icKeyValue USE-INDEX HostTable NO-ERROR.          
END PROCEDURE.

PROCEDURE local-find-NEXT:
  FIND NEXT Counter NO-LOCK WHERE 
            Counter.Brand = gcBrand AND 
            Counter.HostTable = icHostTable AND
            Counter.KeyValue  = icKeyValue USE-INDEX HostTable NO-ERROR.          
END PROCEDURE.

PROCEDURE local-find-PREV:
  FIND PREV Counter NO-LOCK WHERE 
            Counter.Brand = gcBrand AND 
            Counter.HostTable = icHostTable AND
            Counter.KeyValue  = icKeyValue USE-INDEX HostTable NO-ERROR.          
END PROCEDURE.


PROCEDURE local-disp-row:

       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
         Counter.CounterType 
         lcCTypeName
         Counter.CounterAmt   
         Counter.BeginStamp   
         Counter.EndStamp 
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   fCTName(Counter.CounterType).

END PROCEDURE.


PROCEDURE local-UPDATE-record.
      
   RUN local-find-others.
   PAUSE 0.
   DISP
     Counter.Brand
     Counter.CounterSeq 
     Counter.HostTable
     Counter.KeyValue
     Counter.CounterType
     lcCTypeName
     Counter.CounterAmt
     Counter.BeginStamp
     Counter.EndStamp 
   WITH FRAME lis.
     
   IF lcRight NE "RW" THEN RETURN.
      
   FIND CURRENT Counter NO-LOCK NO-ERROR.
   
   UPDATE_LOOP:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
      ehto = 9.
      RUN Syst/ufkey.
   
      DISP Counter.CounterAmt .

      PROMPT-FOR 
         Counter.CounterAmt
      WITH FRAME lis EDITING:
      
         READKEY.
               
        IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
           
            IF FRAME-FIELD = "CounterAmt" THEN DO:     
               IF INPUT Counter.CounterAmt NE "" THEN DO:
               END.
            END.
        END.
         
        ON GO OF FRAME lis DO:
            
            FIND CURRENT Counter NO-LOCK.

            IF CURRENT-CHANGED Counter THEN DO:
               MESSAGE 
                  "This record has been changed elsewhere while updating." 
               VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".
            END. 
            ELSE DO: 
               FIND CURRENT Counter EXCLUSIVE-LOCK.
               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCounter).
               ASSIGN
                  Counter.CounterAmt.
               IF llDoEvent THEN RUN StarEventMakeModifyEvent (lhCounter).
            END.

        END.
         
        APPLY LASTKEY.

      END. /* editing */
      
      LEAVE.

   END. /* repeat */
      
   FIND CURRENT Counter NO-LOCK.
      
   DISP Counter.CounterAmt  WITH FRAME lis.

END PROCEDURE.
         
fCleanEventObjects().
