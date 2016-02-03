/* ----------------------------------------------------------------------
  MODULE .......: BRTestQRow.p
  TASK .........: UPDATEs table BRTestQRow
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'BRTestQRow'}
{Syst/eventval.i}
{Func/timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhBRTestQRow AS HANDLE NO-UNDO.
   lhBRTestQRow = BUFFER BRTestQRow:HANDLE.
   RUN StarEventInitialize(lhBRTestQRow).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhBRTestQRow).
   END.

END.

DEF INPUT PARAMETER iiBRTestQueueID AS INT  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR liQRowID       AS INT                  NO-UNDO.
DEF VAR liBRTestCaseID AS INT                  NO-UNDO.
DEF VAR lcDescription  AS CHAR                 NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 3.
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

DEF VAR lcField        AS CHAR NO-UNDO. 
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR liSeq          AS INT  NO-UNDO.
DEF VAR lcConfName     AS CHAR NO-UNDO.
DEF VAR liUpdateRow    AS INT  NO-UNDO INIT 4. 
DEF VAR lcTestCase     AS CHAR NO-UNDO.
DEF VAR lcDateOperator AS CHAR NO-UNDO.

DEF TEMP-TABLE ttQRow NO-UNDO LIKE BRTestQRow  
   FIELD CaseDescription AS CHAR
   INDEX BRTestQRowID BRTestQRowID
   INDEX BRTestCaseID BRTestCaseID
   INDEX CaseDescription CaseDescription.


FORM
    ttQRow.BRTestQRowID 
    ttQRow.BRTestCaseID
    ttQRow.CaseDescription FORMAT "X(30)" COLUMN-LABEL "Description"
    ttQRow.Active
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " CASE ROWS OF " + STRING(iiBRTestQueueID) + " "
    FRAME sel.

FORM
    BRTestQRow.BRTestQueueID     COLON 25
       lcConfName NO-LABEL FORMAT "X(40)" SKIP
    BRTestQRow.BRTestQRowID COLON 25
    BRTestQRow.Active           COLON 25 SKIP(1)
    BRTestQRow.BRTestCaseID      COLON 25 
       ttQRow.CaseDescription NO-LABEL FORMAT "X(40)" SKIP
    BRTestQRow.CaseQty        COLON 25
WITH  OVERLAY ROW liUpdateRow centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Row ID:" liQRowID FORMAT ">>>>>>>9" 
    HELP "Enter ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND ROW ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM 
    "Case ID:" liBRTestCaseID FORMAT ">>>>>>>9" 
    HELP "Enter ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CASE ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FORM 
    "Description:" lcDescription FORMAT "X(30)" 
    HELP "Enter description"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Description "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.



FUNCTION fTestCase RETURNS CHAR
   (iiTestCaseID AS INT):

   lcTestCase = "".
   FIND FIRST BRTestCase WHERE BRTestCase.BRTestCaseID = iiTestCaseID
      NO-LOCK NO-ERROR.
   IF AVAILABLE BRTestCase THEN lcTestCase = BRTestCase.Description.

   RETURN lcTestCase.
   
END FUNCTION.



FIND FIRST BRTestQueue WHERE BRTestQueue.BRTestQueueID = iiBRTestQueueID 
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE BRTestQueue THEN DO:
   MESSAGE "Test queue not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
lcConfName = BRTestQueue.Description.

RUN pRowsToTempTable(iiBRTestQueueID).

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE ttQRow THEN ASSIGN
   Memory       = recid(ttQRow)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rows available" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a BRTestQRow  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANS WITH FRAME lis:

           DISPLAY iiBRTestQueueID @ BRTestQRow.BRTestQueueID.

           liSeq = 1.
           FIND LAST BRTestQRow USE-INDEX BRTestQRowID 
              NO-LOCK NO-ERROR.
           IF AVAILABLE BRTestQRow THEN 
              liSeq = BRTestQRow.BRTestQRowID + 1.
           
           CREATE BRTestQRow.
           ASSIGN 
              BRTestQRow.BRTestQueueID  = iiBRTestQueueID
              BRTestQRow.BRTestQRowID  = liSeq
              BRTestQRow.Active = TRUE.
           CREATE ttQRow.
           BUFFER-COPY BRTestQRow TO ttQRow.
 
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              BRTestQRow.BRTestCaseID = 0 THEN DO:
                IF AVAILABLE ttQRow THEN DELETE ttQRow.
                UNDO add-row, LEAVE add-row.
           END.     

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBRTestQRow).

           ASSIGN
           Memory = recid(ttQRow)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      RUN pRowsToTempTable(iiBRTestQueueID).
        
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      Memory = recid(ttQRow).
            
      IF NOT AVAILABLE ttQRow THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ttQRow WHERE recid(ttQRow) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttQRow THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttQRow).
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
        ufk   = 0
        ufk[1] = 2580
        ufk[2] = 2581
        ufk[3] = 2582
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttQRow.BRTestQRowID ;(uchoose.i;) 
           NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttQRow.BRTestQRowID 
           WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ttQRow.BRTestCaseID ;(uchoose.i;) 
           NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttQRow.BRTestCaseID 
           WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW ttQRow.CaseDescription ;(uchoose.i;) 
           NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttQRow.CaseDescription 
           WITH FRAME sel.
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
        FIND ttQRow WHERE recid(ttQRow) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttQRow THEN
              ASSIGN FIRSTrow = i Memory = recid(ttQRow).
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
           IF NOT AVAILABLE ttQRow THEN DO:
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
                rtab[1] = recid(ttQRow)
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
           IF NOT AVAILABLE ttQRow THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttQRow).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttQRow WHERE recid(ttQRow) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttQRow THEN DO:
           Memory = recid(ttQRow).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttQRow THEN Memory = recid(ttQRow).
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
           FIND ttQRow WHERE recid(ttQRow) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". run ufcolor.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        PAUSE 0.
        CLEAR FRAME f1.
        SET liQRowID WITH FRAME f1.
        HIDE FRAME f1 NO-PAUSE.
       
        IF liQRowID > 0 THEN DO:
           FIND FIRST ttQRow WHERE ttQRow.BRTestQrowID >= liQRowID NO-ERROR.

           IF NOT AVAILABLE ttQRow THEN DO:
              BELL.
              MESSAGE "NOT FOUND".
              PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ASSIGN order = 1 memory = recid(ttQRow) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". run ufcolor.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        PAUSE 0.
        CLEAR FRAME f2.
        SET liBRTestCaseID WITH FRAME f2.
        HIDE FRAME f2 NO-PAUSE.
       
        IF liBRTestCaseID > 0 THEN DO:
           FIND FIRST ttQRow WHERE ttQRow.BRTestCaseID >= liBRTestCaseID 
              NO-ERROR.

           IF NOT AVAILABLE ttQRow THEN DO:
              BELL.
              MESSAGE "NOT FOUND".
              PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ASSIGN order = 2 memory = recid(ttQRow) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Search-2 */

     /* Search BY column 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". run ufcolor.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        PAUSE 0.
        CLEAR FRAME f3.
        SET lcDescription WITH FRAME f3.
        HIDE FRAME f3 NO-PAUSE.
       
        IF lcDescription > "" THEN DO:
           FIND FIRST ttQRow WHERE ttQRow.CaseDescription >= lcDescription 
              NO-ERROR.

           IF NOT AVAILABLE ttQRow THEN DO:
              BELL.
              MESSAGE "NOT FOUND".
              PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ASSIGN order = 3 memory = recid(ttQRow) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANS:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST BRTestQResultRow WHERE 
           BRTestQResultRow.BRTestQRowID = ttQRow.BRTestQRowID)
       THEN DO:
          MESSAGE "Test results exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          ttQRow.BRTestQRowID
          ttQRow.BRTestCaseID.

       RUN local-find-NEXT.
       IF AVAILABLE ttQRow THEN Memory = recid(ttQRow).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttQRow THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttQRow).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          ttQRow.BRTestQRowID
          ttQRow.BRTestCaseID.
       
       IF ok THEN DO:

           RUN local-find-others.
           FIND CURRENT BRTestQRow EXCLUSIVE-LOCK.
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhBRTestQRow).

           DELETE BRTestQRow.
           DELETE ttQRow.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ttQRow THEN DO:
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
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       RUN local-find-others.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBRTestQRow).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndTS */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBRTestQRow).

       RUN local-disp-row.
       xrecid = recid(ttQRow).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttQRow) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttQRow) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttQRow WHERE recid(ttQRow) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttQRow WHERE recid(ttQRow) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF order = 1 THEN FIND FIRST ttQRow USE-INDEX BRTestQRowID NO-ERROR.
   ELSE IF order = 2 THEN FIND FIRST ttQRow USE-INDEX BRTestCaseID NO-ERROR.
   ELSE IF order = 3 THEN FIND FIRST ttQRow USE-INDEX CaseDescription NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST ttQRow USE-INDEX BRTestQRowID NO-ERROR.
   ELSE IF order = 2 THEN FIND LAST ttQRow USE-INDEX BRTestCaseID NO-ERROR.
   ELSE IF order = 3 THEN FIND LAST ttQRow USE-INDEX CaseDescription NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT ttQRow USE-INDEX BRTestQRowID NO-ERROR.
   ELSE IF order = 2 THEN FIND NEXT ttQRow USE-INDEX BRTestCaseID NO-ERROR.
   ELSE IF order = 3 THEN FIND NEXT ttQRow USE-INDEX CaseDescription NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV ttQRow USE-INDEX BRTestQRowID NO-ERROR.
   ELSE IF order = 2 THEN FIND PREV ttQRow USE-INDEX BRTestCaseID NO-ERROR.
   ELSE IF order = 3 THEN FIND PREV ttQRow USE-INDEX CaseDescription NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       ttQRow.BRTestQRowID
       ttQRow.BRTestCaseID
       ttQRow.CaseDescription
       ttQRow.Active
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   FIND FIRST BRTestQRow WHERE
      BRTestQRow.BRTestQRowID = ttQRow.BRTestQRowID NO-LOCK.
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      IF NOT NEW BRTestQRow THEN 
         RUN local-find-others.
      
      DISP 
         BRTestQRow.BRTestQueueID        
         lcConfName
         BRTestQRow.Active
         BRTestQRow.BRTestQRowID       
         BRTestQRow.BRTestCaseID
         ttQRow.CaseDescription
         BRTestQRow.CaseQty
      WITH FRAME lis.

      IF NOT NEW BRTestQRow THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
      END.
      ELSE toimi = 1.
      
      IF toimi = 1 THEN DO:

         UpdateField:
         REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
            FIND CURRENT BRTestQRow EXCLUSIVE-LOCK.
            ehto = 9.
            RUN ufkey.
         
            UPDATE
               BRTestQRow.Active
               BRTestQRow.BRTestCaseID WHEN NEW BRTestQRow
               BRTestQRow.CaseQty
            WITH FRAME lis EDITING:
 
               READKEY.
       
               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
               DO WITH FRAME lis:
                  PAUSE 0.

                  IF FRAME-FIELD = "BRTestCaseID" THEN DO:
                     ttQRow.CaseDescription = 
                        fTestCase(INPUT INPUT BRTestQRow.BRTestCaseID).
                     IF ttQRow.CaseDescription = "" THEN DO:
                        MESSAGE "Unknown test case"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                     DISPLAY ttQRow.CaseDescription WITH FRAME lis.
                  END.
               END.
            
               APPLY LASTKEY.
            END.

            LEAVE UpdateField.
         END.

         IF NEW BRTestQRow THEN LEAVE.
         
      END.

      ELSE IF toimi = 8 THEN LEAVE. 
   END.
   
END PROCEDURE.

PROCEDURE pRowsToTempTable:

   DEF INPUT PARAMETER iiBRTestQueueID AS INT  NO-UNDO.
   
   EMPTY TEMP-TABLE ttQRow.
   
   FOR EACH BRTestQRow NO-LOCK WHERE
            BRTestQRow.BRTestQueueID = iiBRTestQueueID:
      CREATE ttQRow.
      BUFFER-COPY BRTestQRow TO ttQRow.
      
      ttQRow.CaseDescription = fTestCase(BRTestQRow.BRTestCaseID).
   END.

END PROCEDURE.

