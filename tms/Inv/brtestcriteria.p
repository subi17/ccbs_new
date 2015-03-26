/* ----------------------------------------------------------------------
  MODULE .......: BRTestCriteria.p
  TASK .........: UPDATEs table BRTestCriteria
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 06.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'BRTestCriteria'}
{eventval.i}
{timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhBRTestCriteria AS HANDLE NO-UNDO.
   lhBRTestCriteria = BUFFER BRTestCriteria:HANDLE.
   RUN StarEventInitialize(lhBRTestCriteria).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhBRTestCriteria).
   END.

END.

DEF INPUT PARAMETER iiBRTestQueueID AS INT  NO-UNDO.
DEF INPUT PARAMETER iiBRTestCaseID  AS INT  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
DEF VAR liUpdateRow    AS INT  NO-UNDO INIT 3. 
DEF VAR lcOperator     AS CHAR NO-UNDO.
DEF VAR lcCritOwner    AS CHAR NO-UNDO.
DEF VAR lcQueueName    AS CHAR NO-UNDO.
DEF VAR lcTitle        AS CHAR NO-UNDO.

FORM
    BRTestCriteria.BRTestCriteriaID 
    BRTestCriteria.CriteriaTable FORMAT "X(15)"
    BRTestCriteria.CriteriaField
    BRTestCriteria.ROValueIncl FORMAT "X(2)" COLUMN-LABEL "RO"
    BRTestCriteria.ValueIncluded FORMAT "X(12)" COLUMN-LABEL "Value"
    BRTestCriteria.Active
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " CRITERIA OF " + lcTitle + " "
    FRAME sel.

FORM
    BRTestCriteria.BRTestQueueID    COLON 25
       lcQueueName NO-LABEL FORMAT "X(30)" SKIP
    BRTestCriteria.BRTestCaseID     COLON 25
       lcConfName NO-LABEL FORMAT "X(30)" SKIP
    BRTestCriteria.BRTestCriteriaID COLON 25
    BRTestCriteria.Active           COLON 25 SKIP(1)
    BRTestCriteria.CriteriaTable    COLON 25
    BRTestCriteria.CriteriaField    COLON 25 FORMAT "X(15)"
    BRTestCriteria.ROValueIncl      COLON 25 
       lcOperator NO-LABEL FORMAT "X(30)" SKIP
    BRTestCriteria.ValueIncluded    COLON 25 FORMAT "X(256)" VIEW-AS FILL-IN SIZE 25 BY 1
    BRTestCriteria.Setting          COLON 25 FORMAT "X(256)" VIEW-AS FILL-IN SIZE 25 BY 1
    BRTestCriteria.EventDateFrom    COLON 25 FORMAT "X(256)" VIEW-AS FILL-IN SIZE 25 BY 1
    BRTestCriteria.EventDateTo      COLON 25 FORMAT "X(256)" VIEW-AS FILL-IN SIZE 25 BY 1
WITH  OVERLAY ROW liUpdateRow centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fOperator RETURNS LOGIC
   (icOperator AS CHAR):

   lcOperator = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "BRTestCase",
                                 "RelationalOperator",
                                 icOperator).
   DISP lcOperator WITH FRAME lis.

   RETURN (lcOperator > "").
   
END FUNCTION.

IF iiBRTestQueueID > 0 THEN DO:
   FIND FIRST BRTestQueue WHERE BRTestQueue.BRTestQueueID = iiBRTestQueueID
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BRTestQueue THEN DO:
      MESSAGE "Test queue not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   ASSIGN
      lcCritOwner = "BRTestQueue"
      lcTitle = "QUEUE " + STRING(iiBRTestQueueID)
      lcQueueName = BRTestQueue.Description.
END.

ELSE DO:
   FIND FIRST BRTestCase WHERE BRTestCase.BRTestCaseID = iiBRTestCaseID 
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BRTestCase THEN DO:
      MESSAGE "Test case not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   ASSIGN
      lcCritOwner = "BRTestCase"
      lcTitle = "CASE " + STRING(iiBRTestCaseID)
      lcConfName = BRTestCase.Description.
END.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE BRTestCriteria THEN ASSIGN
   Memory       = recid(BRTestCriteria)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No criteria available" VIEW-AS ALERT-BOX INFORMATION.
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

   IF must-add THEN DO:  /* Add a BRTestCriteria  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANS WITH FRAME lis:

           IF iiBRTestQueueID > 0 THEN 
              DISPLAY iiBRTestQueueID @ BRTestCriteria.BRTestQueueID.
           ELSE DISPLAY iiBRTestCaseID @ BRTestCriteria.BRTestCaseID.

           liSeq = 1.
           FIND LAST BRTestCriteria USE-INDEX BRTestCriteriaID 
              NO-LOCK NO-ERROR.
           IF AVAILABLE BRTestCriteria THEN 
              liSeq = BRTestCriteria.BRTestCriteriaID + 1.
           
           CREATE BRTestCriteria.
           ASSIGN 
              BRTestCriteria.BRTestQueueID = iiBRTestQueueID
              BRTestCriteria.BRTestCaseID  = iiBRTestCaseID
              BRTestCriteria.BRTestCriteriaID  = liSeq
              BRTestCriteria.EventDateFrom = "#BillPeriodBeg"
              BRTestCriteria.EventDateTo   = "#BillPeriodEnd"
              BRTestCriteria.CriteriaOwner = lcCritOwner.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              BRTestCriteria.CriteriaTable = ""  OR
              BRTestCriteria.CriteriaField = "" THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBRTestCriteria).

           ASSIGN
           Memory = recid(BRTestCriteria)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE BRTestCriteria THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND BRTestCriteria WHERE recid(BRTestCriteria) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE BRTestCriteria THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(BRTestCriteria).
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
        CHOOSE ROW BRTestCriteria.BRTestCriteriaID ;(uchoose.i;) 
           NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) BRTestCriteria.BRTestCriteriaID 
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
        FIND BRTestCriteria WHERE recid(BRTestCriteria) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE BRTestCriteria THEN
              ASSIGN FIRSTrow = i Memory = recid(BRTestCriteria).
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
           IF NOT AVAILABLE BRTestCriteria THEN DO:
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
                rtab[1] = recid(BRTestCriteria)
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
           IF NOT AVAILABLE BRTestCriteria THEN DO:
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
              rtab[FRAME-DOWN] = recid(BRTestCriteria).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND BRTestCriteria WHERE recid(BRTestCriteria) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE BRTestCriteria THEN DO:
           Memory = recid(BRTestCriteria).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE BRTestCriteria THEN Memory = recid(BRTestCriteria).
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
           FIND BRTestCriteria WHERE recid(BRTestCriteria) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

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

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          BRTestCriteria.BRTestCriteriaID
          BRTestCriteria.CriteriaTable
          BRTestCriteria.CriteriaField.

       RUN local-find-NEXT.
       IF AVAILABLE BRTestCriteria THEN Memory = recid(BRTestCriteria).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE BRTestCriteria THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(BRTestCriteria).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          BRTestCriteria.BRTestCriteriaID
          BRTestCriteria.CriteriaTable
          BRTestCriteria.CriteriaField.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhBRTestCriteria).

           DELETE BRTestCriteria.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE BRTestCriteria THEN DO:
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
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBRTestCriteria).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndTS */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBRTestCriteria).

       RUN local-disp-row.
       xrecid = recid(BRTestCriteria).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(BRTestCriteria) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(BRTestCriteria) must-print = TRUE.
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
      FIND BRTestCriteria WHERE recid(BRTestCriteria) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND BRTestCriteria WHERE recid(BRTestCriteria) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF iiBRTestQueueID > 0 THEN DO:
      IF order = 1 THEN FIND FIRST BRTestCriteria USE-INDEX BRTestQueueID 
         WHERE BRTestCriteria.BRTestQueueID = iiBRTestQueueID NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND FIRST BRTestCriteria USE-INDEX BRTestCaseID 
          WHERE BRTestCriteria.BRTestCaseID = iiBRTestCaseID AND
                BRTestCriteria.CriteriaOwner = "BRTestCase" NO-LOCK NO-ERROR.
   END. 
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiBRTestQueueID > 0 THEN DO:
      IF order = 1 THEN FIND LAST BRTestCriteria USE-INDEX BRTestQueueID 
         WHERE BRTestCriteria.BRTestQueueID = iiBRTestQueueID NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND LAST BRTestCriteria USE-INDEX BRTestCaseID 
          WHERE BRTestCriteria.BRTestCaseID = iiBRTestCaseID AND
                BRTestCriteria.CriteriaOwner = "BRTestCase" NO-LOCK NO-ERROR.
   END. 
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiBRTestQueueID > 0 THEN DO:
      IF order = 1 THEN FIND NEXT BRTestCriteria USE-INDEX BRTestQueueID 
         WHERE BRTestCriteria.BRTestQueueID = iiBRTestQueueID NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND NEXT BRTestCriteria USE-INDEX BRTestCaseID 
          WHERE BRTestCriteria.BRTestCaseID = iiBRTestCaseID AND
                BRTestCriteria.CriteriaOwner = "BRTestCase" NO-LOCK NO-ERROR.
   END. 
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiBRTestQueueID > 0 THEN DO:
      IF order = 1 THEN FIND PREV BRTestCriteria USE-INDEX BRTestQueueID 
         WHERE BRTestCriteria.BRTestQueueID = iiBRTestQueueID NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND PREV BRTestCriteria USE-INDEX BRTestCaseID 
          WHERE BRTestCriteria.BRTestCaseID = iiBRTestCaseID AND
                BRTestCriteria.CriteriaOwner = "BRTestCase" NO-LOCK NO-ERROR.
   END. 
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       BRTestCriteria.BRTestCriteriaID
       BRTestCriteria.CriteriaTable
       BRTestCriteria.CriteriaField
       BRTestCriteria.ROValueIncl
       BRTestCriteria.ValueIncluded
       BRTestCriteria.Active
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         BRTestCriteria.BRTestQueueID
         lcQueueName
         BRTestCriteria.BRTestCaseID        
         lcConfName
         BRTestCriteria.Active
         BRTestCriteria.BRTestCriteriaID       
         BRTestCriteria.CriteriaTable
         BRTestCriteria.CriteriaField
         BRTestCriteria.ROValueIncluded
         BRTestCriteria.ValueIncluded
         BRTestCriteria.Setting
         BRTestCriteria.EventDateFrom
         BRTestCriteria.EventDateTo
      WITH FRAME lis.

      fOperator(BRTestCriteria.ROValueIncluded).
      
      IF NOT NEW BRTestCriteria THEN DO:
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
                
            FIND CURRENT BRTestCriteria EXCLUSIVE-LOCK.
            ehto = 9.
            RUN ufkey.
         
            UPDATE
               BRTestCriteria.Active
               BRTestCriteria.CriteriaTable WHEN NEW BRTestCriteria
               BRTestCriteria.CriteriaField WHEN NEW BRTestCriteria
               BRTestCriteria.ROValueIncluded
               BRTestCriteria.ValueIncluded
               BRTestCriteria.Setting
               BRTestCriteria.EventDateFrom
               BRTestCriteria.EventDateTo
            WITH FRAME lis EDITING:   
 
               READKEY.
       
               IF KEYLABEL(LASTKEY) = "F9" AND 
                  LOOKUP(FRAME-FIELD,
                            "CriteriaTable,CriteriaField,ROValueIncluded," +
                            "Setting") > 0
               THEN DO:

                  IF FRAME-FIELD = "CriteriaTable" THEN DO:
                     RUN h-tmscodes(INPUT "BRTestCriteria", 
                                          "CriteriaTable", 
                                          "BRTest",  
                                    OUTPUT lcCode).
             
                     IF lcCode ne "" AND lcCode NE ? THEN
                        DISPLAY lcCode @ BRTestCriteria.CriteriaTable 
                           WITH FRAME lis.   
                  END.
                  ELSE IF FRAME-FIELD = "CriteriaField" THEN DO:
                     RUN h-tmscodes(INPUT "BRTestCriteria", 
                                          "CField" + 
                                           INPUT BRTestCriteria.CriteriaTable, 
                                          "BRTest", 
                                    OUTPUT lcCode).
             
                     IF lcCode ne "" AND lcCode NE ? THEN
                        DISPLAY lcCode @ BRTestCriteria.CriteriaField 
                           WITH FRAME lis.   
                  END.
                  ELSE IF FRAME-FIELD = "Setting" THEN DO:
                     RUN h-tmscodes(INPUT "BRTestCriteria", 
                                          "Setting" + 
                                           INPUT BRTestCriteria.CriteriaTable, 
                                          "BRTest", 
                                    OUTPUT lcCode).
             
                     IF lcCode ne "" AND lcCode NE ? THEN
                        DISPLAY lcCode @ BRTestCriteria.Setting 
                           WITH FRAME lis.   
                  END.
                  ELSE IF FRAME-FIELD = "ROValueIncluded" THEN DO:
                     RUN h-tmscodes(INPUT "BRTestCase",
                                          "RelationalOperator",
                                          "BRTest",  
                                    OUTPUT lcCode).
                     IF lcCode ne "" AND lcCode NE ? THEN
                        DISPLAY lcCode @ BRTestCriteria.ROValueIncluded
                           WITH FRAME lis.   
                  END.
                  
                  ehto = 9.
                  RUN ufkey.
                  NEXT. 
               END.

               ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
               DO WITH FRAME lis:
                  PAUSE 0.

                  IF FRAME-FIELD = "CriteriaTable" THEN DO:
                     IF DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                INPUT "BRTestCriteria",
                                INPUT "CriteriaTable",
                                INPUT INPUT BRTestCriteria.CriteriaTable) = ""
                     THEN DO:
                        MESSAGE "Unknown table"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.                      
                  END.
                                      
                  ELSE IF FRAME-FIELD = "CriteriaField" THEN DO:
                     IF DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                INPUT "BRTestCriteria",
                                INPUT "CField" + 
                                       INPUT BRTestCriteria.CriteriaTable,
                                INPUT INPUT BRTestCriteria.CriteriaField) = ""
                     THEN DO:
                        MESSAGE "Unknown field"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.                      
                  END.

                  ELSE IF FRAME-FIELD = "Setting" THEN DO:
                     IF INPUT FRAME lis BRTestCriteria.Setting > "" AND
                        DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                INPUT "BRTestCriteria",
                                INPUT "Setting" + 
                                       INPUT BRTestCriteria.CriteriaTable,
                                INPUT INPUT BRTestCriteria.Setting) = ""
                     THEN DO:
                        MESSAGE "Unknown setting"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.                      
                  END.

                  ELSE IF FRAME-FIELD = "ROValueIncluded" THEN DO:
                     IF NOT fOperator(
                           INPUT INPUT BRTestCriteria.ROValueIncluded) 
                     THEN DO:
                        MESSAGE "Unknown operator"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.

                     IF LOOKUP(INPUT BRTestCriteria.ROValueIncluded,
                               "EQ,NE") = 0 
                     THEN DO:
                        IF LOOKUP(INPUT BRTestCriteria.CriteriaTable,
                                  "MobCDR,InvRowCounter") = 0  OR
                        (INPUT BRTestCriteria.CriteriaTable = "MobCDR" AND
                           LOOKUP(INPUT BRTestCriteria.CriteriaField,
                                  "Qty,BillDur,DataAmt,Amount") = 0) OR
                        (INPUT BRTestCriteria.CriteriaTable = "InvRowCounter" 
                           AND
                         LOOKUP(INPUT BRTestCriteria.CriteriaField,
                               "Quantity,Duration,DataAmt,Amount") = 0)
                        THEN DO:        
                           MESSAGE "For this table/field combination only"
                                   "EQ and NE are allowed"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT.
                        END.   
                     END.
                  END.

               END.
            
               APPLY LASTKEY.
            END.

            IF BRTestCriteria.EventDateFrom = "" OR 
               BRTestCriteria.EventDateTo = "" THEN DO:
                  MESSAGE "Period is mandatory"
                  VIEW-AS ALERT-BOX INFORMATION.
                  NEXT.
            END.
            
            LEAVE UpdateField.
         END.
            
         IF NEW BRTestCriteria THEN LEAVE.
         
      END.

      ELSE IF toimi = 8 THEN LEAVE. 
   END.
   
END PROCEDURE.


