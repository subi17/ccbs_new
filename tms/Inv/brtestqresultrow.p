/* ----------------------------------------------------------------------
  MODULE .......: BRTestQResultRow.p
  TASK .........: UPDATEs table BRTestQResultRow
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'BRTestQResultRow'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhBRTestQResultRow AS HANDLE NO-UNDO.
   lhBRTestQResultRow = BUFFER BRTestQResultRow:HANDLE.
   RUN StarEventInitialize(lhBRTestQResultRow).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhBRTestQResultRow).
   END.

END.

DEF INPUT PARAMETER iiBRTestQResultID AS INT  NO-UNDO.

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
DEF VAR liUpdateRow    AS INT  NO-UNDO INIT 4. 
DEF VAR lcTestCase     AS CHAR NO-UNDO.
DEF VAR liTestCaseID   AS INT  NO-UNDO.
DEF VAR lcCustName     AS CHAR NO-UNDO.
DEF VAR lcCLI          AS CHAR NO-UNDO. 

FORM
    BRTestQResultRow.QResultRowID 
    liTestCaseID     FORMAT ">>>>>>>9" COLUMN-LABEL "Test Case"
    lcTestCase       FORMAT "X(20)" COLUMN-LABEL "Description"
    BRTestQResultRow.InvCust 
    BRTestQResultRow.TestResult FORMAT "X(20)"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.CUICommon:cfc)   
    TITLE COLOR VALUE(Syst.CUICommon:ctc) 
       " RESULT ROWS OF " + STRING(iiBRTestQResultID) + " "
    FRAME sel.

FORM
    BRTestQResultRow.BRTestQResultID  COLON 25
    BRTestQResultRow.QResultRowID     COLON 25
    BRTestQResultRow.BRTestQRowID     COLON 25
    liTestCaseID                      COLON 25
       FORMAT ">>>>>>>9" LABEL "Test Case"
    lcTestCase                        COLON 25  
       FORMAT "X(30)" LABEL "Description" SKIP(1)
    BRTestQResultRow.InvCust          COLON 25
       lcCustName NO-LABEL FORMAT "X(30)" SKIP
    BRTestQResultRow.MsSeq            COLON 25
       lcCLI NO-LABEL FORMAT "X(15)" SKIP
    BRTestQResultRow.ExtInvID         COLON 25
    BRTestQResultRow.InvNum FORMAT ">>>>>>>>9" COLON 25 SKIP(1)
    BRTestQResultRow.ResultValue      COLON 25 
    BRTestQResultRow.TestResult       COLON 25 
WITH  OVERLAY ROW liUpdateRow centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fTestCase RETURNS LOGIC
   (iiBRTestQRowID AS INT):

   ASSIGN 
      lcTestCase = ""
      liTestCaseID = 0.
   FOR FIRST BRTestQRow NO-LOCK WHERE
             BRTestQRow.BRTestQRowID = iiBRTestQRowID,
       FIRST BRTestCase NO-LOCK WHERE 
             BRTestCase.BRTestCaseID = BRTestQRow.BRTestCaseID:
      ASSIGN 
         lcTestCase = BRTestCase.Description
         liTestCaseID = BRTestCase.BRTestCaseID.
   END.
   
   RETURN (liTestCaseID > 0).
   
END FUNCTION.



FIND FIRST BRTestQResult WHERE 
   BRTestQResult.BRTestQResultID = iiBRTestQResultID NO-LOCK NO-ERROR.
IF NOT AVAILABLE BRTestQResult THEN DO:
   MESSAGE "Test result not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE BRTestQResultRow THEN ASSIGN
   Memory       = recid(BRTestQResultRow)
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

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND BRTestQResultRow WHERE recid(BRTestQResultRow) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE BRTestQResultRow THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(BRTestQResultRow).
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
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW BRTestQResultRow.QResultRowID {Syst/uchoose.i} 
           NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.CUICommon:ccc) BRTestQResultRow.QResultRowID 
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
        FIND BRTestQResultRow WHERE recid(BRTestQResultRow) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE BRTestQResultRow THEN
              ASSIGN FIRSTrow = i Memory = recid(BRTestQResultRow).
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
           IF NOT AVAILABLE BRTestQResultRow THEN DO:
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
                rtab[1] = recid(BRTestQResultRow)
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
           IF NOT AVAILABLE BRTestQResultRow THEN DO:
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
              rtab[FRAME-DOWN] = recid(BRTestQResultRow).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND BRTestQResultRow WHERE recid(BRTestQResultRow) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE BRTestQResultRow THEN DO:
           Memory = recid(BRTestQResultRow).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE BRTestQResultRow THEN 
                 Memory = recid(BRTestQResultRow).
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
           FIND BRTestQResultRow WHERE recid(BRTestQResultRow) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBRTestQResultRow).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       Syst.CUICommon:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndTS */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBRTestQResultRow).

       RUN local-disp-row.
       xrecid = recid(BRTestQResultRow).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(BRTestQResultRow) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(BRTestQResultRow) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND BRTestQResultRow WHERE recid(BRTestQResultRow) = 
         rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND BRTestQResultRow WHERE recid(BRTestQResultRow) = 
         rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST BRTestQResultRow USE-INDEX BRTestQResultID WHERE 
        BRTestQResultRow.BRTestQResultID = iiBRTestQResultID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN 
      FIND LAST BRTestQResultRow USE-INDEX BRTestQResultID WHERE 
        BRTestQResultRow.BRTestQResultID = iiBRTestQResultID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT BRTestQResultRow USE-INDEX BRTestQResultID WHERE 
        BRTestQResultRow.BRTestQResultID = iiBRTestQResultID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV BRTestQResultRow USE-INDEX BRTestQResultID WHERE 
        BRTestQResultRow.BRTestQResultID = iiBRTestQResultID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       BRTestQResultRow.QResultRowID
       liTestCaseID
       lcTestCase
       BRTestQResultRow.InvCust
       BRTestQResultRow.TestResult
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   fTestCase(BRTestQResultRow.BRTestQRowID).
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      FIND FIRST Customer WHERE Customer.CustNum = BRTestQResultRow.InvCust
         NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN
         lcCustName = Func.Common:mDispCustName(BUFFER Customer).
      ELSE lcCustName = "".
      
      FIND FIRST MobSub WHERE MobSub.MsSeq = BRTestQResultRow.MsSeq 
         NO-LOCK NO-ERROR.
      IF AVAILABLE MobSub THEN lcCLI = MobSub.CLI.
      ELSE DO:
         FIND FIRST TermMobSub WHERE TermMobSub.MsSeq = BRTestQResultRow.MsSeq
            NO-LOCK NO-ERROR.
         IF AVAILABLE TermMobSub THEN lcCLI = TermMobSub.CLI.
      END.   
      
      DISP 
         BRTestQResultRow.BRTestQResultID        
         BRTestQResultRow.QResultRowID       
         BRTestQResultRow.BRTestQRowID
         liTestCaseID
         lcTestCase
         BRTestQResultRow.ResultValue
         BRTestQResultRow.TestResult
         BRTestQResultRow.InvCust 
         lcCustName
         BRTestQResultRow.MsSeq
         lcCLI
         BRTestQResultRow.ExtInvID
         BRTestQResultRow.InvNum
      WITH FRAME lis.

      ASSIGN 
         ufk    = 0
         ufk[8] = 8
         ehto   = 0.
         
      RUN Syst/ufkey.p.
      
      IF Syst.CUICommon:toimi = 8 THEN LEAVE. 
   END.
   
END PROCEDURE.


