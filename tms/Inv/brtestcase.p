/* ----------------------------------------------------------------------
  MODULE .......: BRTestCase
  TASK .........: UPDATEs table BRTestCase
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 06.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable BRTestCase

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'BRTestCase'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhBRTestCase AS HANDLE NO-UNDO.
   lhBRTestCase = BUFFER BRTestCase:HANDLE.
   RUN StarEventInitialize(lhBRTestCase).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhBRTestCase).
   END.

END.

DEF shared VAR siirto AS CHAR.

DEF VAR lcName        AS CHAR                   NO-UNDO.
DEF VAR liTestCaseID  AS INT                    NO-UNDO.
DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 2.
DEF VAR ufkey         AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                    NO-UNDO  init 0.
DEF VAR pr-order      AS INT                    NO-UNDO.
DEF VAR Memory        AS RECID                  NO-UNDO.
DEF VAR RowNo         AS INT                    NO-UNDO.
DEF VAR must-print    AS LOG                    NO-UNDO.
DEF VAR must-add      AS LOG                    NO-UNDO.
DEF VAR ac-hdr        AS CHAR                   NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24        NO-UNDO.
DEF VAR i             AS INT                    NO-UNDO.
DEF VAR ok            AS log format "Yes/No"    NO-UNDO.

DEF VAR lcField    AS CHAR NO-UNDO. 
DEF VAR lcCode     AS CHAR NO-UNDO. 
DEF VAR lcBIName   AS CHAR NO-UNDO.
DEF VAR lcOperator AS CHAR NO-UNDO.
DEF VAR liCopyBRTestCaseID AS INT  NO-UNDO.
DEF VAR lcTestCase     AS CHAR NO-UNDO. 

FORM
    BRTestCase.BRTestCaseID  FORMAT ">>>>>9" COLUMN-LABEL "ID"
    BRTestCase.Description FORMAT "X(35)"
    BRTestCase.Active
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  BILLING TEST CASE  " +
       string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    BRTestCase.Brand              COLON 25
    BRTestCase.BRTestCaseID       COLON 25
    BRTestCase.Active             COLON 25
    BRTestCase.Description VIEW-AS EDITOR SIZE 60 BY 2 SKIP(1)
    BRTestCase.BillCode           COLON 25 
       lcBIName NO-LABEL FORMAT "X(30)" SKIP
    BRTestCase.RelationalOperator COLON 25
       lcOperator NO-LABEL FORMAT "X(30)" SKIP
    BRTestCase.ResultValue        COLON 25 
    BRTestCase.ResultQty          COLON 25
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand:" lcBrand skip
    "Case ID:" liTestCaseID FORMAT ">>>>>>>9" 
    HELP "Enter ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM 
    "Brand:" lcBrand skip
    "Description:" lcName FORMAT "X(20)" 
    HELP "Enter description"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Description "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FORM
   SKIP(1)
   "Copy criteria from another test case. All table/field" AT 2 SKIP
   "combinations that do not exist on this case are copied." AT 2 SKIP(1)
   liCopyBRTestCaseID AT 2 
      FORMAT ">>>>>>>9"
      LABEL "Copy From"
      HELP "Test case ID from which criteria is copied"
      lcTestCase NO-LABEL FORMAT "X(40)" SKIP(1)
   WITH OVERLAY SIDE-LABELS ROW 10 CENTERED TITLE " COPY CRITERIA "
      FRAME fCopy.


FUNCTION fBIName RETURNS LOGIC
   (icBillCode AS CHAR):

   lcBIName = "".
   IF icBillCode > "" THEN DO: 
      FIND FIRST BillItem WHERE
         BillItem.Brand = gcBrand AND
         BillItem.BillCode = icBillCode NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem THEN lcBIName = BillItem.BIName.
   END.
   
   DISP lcBIName WITH FRAME lis.
   
   RETURN (lcBIName > "").
   
END FUNCTION.

FUNCTION fOperator RETURNS LOGIC
   (icOperator AS CHAR):

   lcOperator = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "BRTestCase",
                                 "RelationalOperator",
                                 icOperator).
   DISP lcOperator WITH FRAME lis.

   RETURN (lcOperator > "").
   
END FUNCTION.


IF gcHelpParam > "" THEN ASSIGN
   FrmRow  = 3
   FrmDown = 11.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE BRTestCase THEN ASSIGN
   Memory       = recid(BRTestCase)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No cases available!" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a BRTestCase  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ BRTestCase.Brand.

           /*
           PROMPT-FOR BRTestCase.ConfName WITH FRAME lis.
           IF INPUT BRTestCase.ConfName = "" THEN UNDO, LEAVE ADD-ROW.
           
           IF CAN-FIND(FIRST BRTestCase WHERE 
                             BRTestCase.Brand = lcBrand AND
                             BRTestCase.ConfName = 
                                       INPUT BRTestCase.ConfName)
           THEN DO:
              MESSAGE "Configuration already exists"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
           */
            
           FIND LAST BRTestCase USE-INDEX BRTestCaseID NO-LOCK NO-ERROR.
           IF AVAILABLE BRTestCase
           THEN i = BRTestCase.BRTestCaseID + 1.
           ELSE i = 1.

           CREATE BRTestCase.
           ASSIGN 
              BRTestCase.Brand        = lcBrand
              BRTestCase.BRTestCaseID = i.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBRTestCase).

           ASSIGN
           Memory = recid(BRTestCase)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE BRTestCase THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND BRTestCase WHERE recid(BRTestCase) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE BRTestCase THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(BRTestCase).
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
        ufk[1] = 2581
        ufk[2] = 2582
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[7] = 0  
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW BRTestCase.BRTestCaseID ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) BRTestCase.BRTestCaseID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW BRTestCase.Description ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) BRTestCase.Description WITH FRAME sel.
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
        FIND BRTestCase WHERE recid(BRTestCase) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE BRTestCase THEN
              ASSIGN FIRSTrow = i Memory = recid(BRTestCase).
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
           IF NOT AVAILABLE BRTestCase THEN DO:
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
                rtab[1] = recid(BRTestCase)
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
           IF NOT AVAILABLE BRTestCase THEN DO:
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
              rtab[FRAME-DOWN] = recid(BRTestCase).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND BRTestCase WHERE recid(BRTestCase) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE BRTestCase THEN DO:
           Memory = recid(BRTestCase).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE BRTestCase THEN Memory = recid(BRTestCase).
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
           FIND BRTestCase WHERE recid(BRTestCase) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           liTestCaseID WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF liTestCaseID > 0 THEN DO:
          FIND FIRST BRTestCase WHERE 
                     BRTestCase.Brand = lcBrand AND
                     BRTestCase.BRTestCaseID >= liTestCaseID
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       SET lcBrand WHEN gcAllBrand 
           lcName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       
       IF lcName > "" THEN DO:
          FIND FIRST BRTestCase WHERE 
                     BRTestCase.Brand = lcBrand AND
                     BRTestCase.Description >= lcName
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

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
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST BRTestCriteria WHERE 
            BRTestCriteria.BRTestCaseID = BRTestCase.BRTestCaseID)
       THEN DO:
          MESSAGE "Criteria exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       IF CAN-FIND(FIRST BRTestQRow WHERE 
           BRTestQRow.BRTestCaseID = BRTestCase.BRTestCaseID)
       THEN DO:
          MESSAGE "Used in a test queue. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       BRTestCase.BRTestCaseID BRTestCase.Description.
        
       RUN local-find-NEXT.
       IF AVAILABLE BRTestCase THEN Memory = recid(BRTestCase).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE BRTestCase THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(BRTestCase).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          BRTestCase.BRTestCaseID BRTestCase.Description.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhBRTestCase).

           DELETE BRTestCase.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE BRTestCase THEN DO:
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
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBRTestCase).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY BRTestCase.BRTestCaseID.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBRTestCase).

       RUN local-disp-row.
       xrecid = recid(BRTestCase).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(BRTestCase) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(BRTestCase) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

IF gcHelpParam > "" THEN DO:
   FIND FIRST BRTestCase WHERE RECID(BRTestCase) = xRecid NO-LOCK.
   siirto = STRING(BRTestCase.BRTestCaseID).
END.
   

ehto = 4.
RUN Syst/ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND BRTestCase WHERE recid(BRTestCase) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND BRTestCase WHERE recid(BRTestCase) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST BRTestCase USE-INDEX BrandID WHERE 
                 BRTestCase.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST BRTestCase USE-INDEX Description WHERE 
                 BRTestCase.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST BRTestCase USE-INDEX BrandID WHERE 
                BRTestCase.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST BRTestCase USE-INDEX Description WHERE 
                BRTestCase.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT BRTestCase USE-INDEX BrandID WHERE 
                BRTestCase.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT BRTestCase USE-INDEX Description WHERE 
                BRTestCase.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV BRTestCase USE-INDEX BrandID WHERE 
                BRTestCase.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV BRTestCase USE-INDEX Description WHERE 
                BRTestCase.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       BRTestCase.BRTestCaseID 
       BRTestCase.Description
       BRTestCase.Active
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR lcValue AS CHAR NO-UNDO.
   
   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP 
         BRTestCase.Brand          
         BRTestCase.BRTestCaseID        
         BRTestCase.Active          
         BRTestCase.BillCode           
         BRTestCase.ResultValue
         BRTestCase.ResultQty
         BRTestCase.RelationalOperator         
         BRTestCase.Description   
      WITH FRAME lis.

      fBIName(BRTestCase.BillCode).
      fOperator(BRTestCase.RelationalOperator).

      IF NEW BRTestCase THEN toimi = 1.

      ELSE DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7    WHEN lcRight = "RW"
            ufk[2] = 9807
            ufk[4] = 1998
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
      END.
                  
      IF toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         FIND CURRENT BRTestCase EXCLUSIVE-LOCK.
      
         ehto = 9.
         RUN Syst/ufkey.
   
         UPDATE
            BRTestCase.Active          
            BRTestCase.Description
            BRTestCase.BillCode           
            BRTestCase.RelationalOperator         
            BRTestCase.ResultValue
            BRTestCase.ResultQty
         WITH FRAME lis EDITING:
 
            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               FRAME-FIELD = "RelationalOperator"
            THEN DO:

               RUN Help/h-tmscodes(INPUT "BRTestCase", /* TableName */
                                    "RelationalOperator",   /* FieldName */
                                    "BRTest",   /* GroupCode */
                              OUTPUT lcCode).
             
               IF lcCode ne "" AND lcCode NE ? THEN
                  DISPLAY lcCode @ BRTestCase.RelationalOperator 
                  WITH FRAME lis.   
                  
               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
               
               IF FRAME-FIELD = "BillCode" THEN DO:
                  IF NOT fBIName(INPUT INPUT BRTestCase.BillCode) THEN DO:
                     MESSAGE "Unknown billing item"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
                  
               ELSE IF FRAME-FIELD = "RelationalOperator" THEN DO:
                  IF NOT fOperator(INPUT INPUT BRTestCase.RelationalOperator) 
                  THEN DO:
                     MESSAGE "Unknown operator"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
            END.
            
            APPLY LASTKEY.
         END.
   
         IF NEW BRTestCase THEN LEAVE MaintMenu.
         LEAVE.
      END.

      ELSE IF toimi = 2 THEN RUN pCopyCriteria(BRTestCase.BRTestCaseID).

      ELSE IF toimi = 4 THEN DO:
         RUN Inv/brtestcriteria.p (0,BRTestCase.BRTestCaseID).
      END.
       
      ELSE IF toimi = 8 THEN LEAVE.  

   END.
   
END PROCEDURE.


PROCEDURE pCopyCriteria:

   DEF INPUT PARAMETER iiCopyToCaseID AS INT  NO-UNDO.
   
   DEF VAR liSeq    AS INT  NO-UNDO.
   DEF VAR liCopied AS INT  NO-UNDO.
   
   DEF BUFFER bCopyFrom FOR BRTestCase.
   DEF BUFFER bCopyCriteria FOR BRTestCriteria.
   
   PAUSE 0.
   VIEW FRAME fCopy.
   
   REPEAT ON ENDKEY UNDO, LEAVE:

      ehto = 9.
      RUN Syst/ufkey.p.
      
      PAUSE 0.
      UPDATE liCopyBRTestCaseID WITH FRAME fCopy.
      FIND FIRST bCopyFrom WHERE bCopyFrom.BRTestCaseID = liCopyBRTestCaseID
         NO-LOCK NO-ERROR.
      IF AVAILABLE bCopyFrom THEN lcTestCase = bCopyFrom.Description.
      ELSE lcTestCase = "".
      DISP lcTestCase WITH FRAME fCopy.
    
      ASSIGN 
         ufk = 0
         ufk[1] = 7
         ufk[5] = 98
         ufk[8] = 8
         ehto = 0.
      RUN Syst/ufkey.p. 
   
      IF toimi = 5 THEN DO:
         IF liCopyBRTestCaseID = 0 OR NOT AVAILABLE bCopyFrom THEN DO:
            MESSAGE "Source for the copying has not been selected"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.

         IF liCopyBRTestCaseID = iiCopyToCaseID THEN DO:
            MESSAGE "Source and target are the same"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         
         liSeq = 0.
         FIND LAST BRTestCriteria USE-INDEX BRTestCriteriaID 
            NO-LOCK NO-ERROR.
         IF AVAILABLE BRTestCriteria THEN 
            liSeq = BRTestCriteria.BRTestCriteriaID.
    
         FOR EACH bCopyCriteria NO-LOCK WHERE
                  bCopyCriteria.BRTestCaseID = bCopyFrom.BRTestCaseID AND
                  bCopyCriteria.Active = TRUE:
          
            IF CAN-FIND(FIRST BRTestCriteria WHERE
               BRTestCriteria.BRTestCaseID = iiCopyToCaseID AND
               BRTestCriteria.Active = TRUE AND
               BRTestCriteria.CriteriaTable = bCopyCriteria.CriteriaTable AND
               BRTestCriteria.CriteriaField = bCopyCriteria.CriteriaField AND
               BRTestCriteria.Setting = bCopyCriteria.Setting)
            THEN NEXT.
               
            CREATE BRTestCriteria.
            ASSIGN 
               BRTestCriteria.BRTestCaseID  = iiCopyToCaseID
               liSeq = liSeq + 1
               BRTestCriteria.BRTestCriteriaID = liSeq
               liCopied = liCopied + 1.
            BUFFER-COPY bCopyCriteria EXCEPT BRTestCaseID BRTestCriteriaID   
               TO BRTestCriteria.
         END.

         MESSAGE liCopied "criteria rows were copied"
         VIEW-AS ALERT-BOX TITLE " DONE ".
         LEAVE.
      END.
      
      ELSE IF toimi = 8 THEN LEAVE.
   END.
   
   HIDE FRAME fCopy NO-PAUSE.
   
END PROCEDURE.

