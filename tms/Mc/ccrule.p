/* ----------------------------------------------------------------------
  MODULE .......: CCRule.P
  TASK .........: CCRule
  APPLICATION ..: TMS
  AUTHOR .......: Koundinya Maddali
  CREATED ......: 23-04-2018
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable CCRule

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ccrule'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCCRule AS HANDLE NO-UNDO.
   lhCCRule = BUFFER CCRule:HANDLE.
   RUN StarEventInitialize(lhCCRule).

   ON F12 ANYWHERE DO:
       RUN Mc/eventview2.p(lhCCRule).
   END.

END.

DEFINE SHARED VARIABLE siirto AS CHARACTER.

DEFINE INPUT PARAMETER icBillCode AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icCategory AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcBillCode LIKE CCRule.BillCode NO-UNDO. 
DEFINE VARIABLE lcCategory LIKE CCRule.Category NO-UNDO.
DEFINE VARIABLE xrecid     AS RECID     INIT ?.
DEFINE VARIABLE FIRSTrow   AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE FrmRow     AS INTEGER   NO-UNDO INIT 1.
DEFINE VARIABLE FrmDown    AS INTEGER   NO-UNDO INIT 15.
DEFINE VARIABLE order      AS INTEGER   NO-UNDO INIT 1.
DEFINE VARIABLE maxOrder   AS INTEGER   NO-UNDO INIT 2.
DEFINE VARIABLE ufkey      AS LOG       NO-UNDO INIT TRUE.
DEFINE VARIABLE delrow     AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE pr-order   AS INTEGER   NO-UNDO.
DEFINE VARIABLE memory     AS RECID     NO-UNDO.
DEFINE VARIABLE RowNo      AS INTEGER   NO-UNDO.
DEFINE VARIABLE must-print AS LOG       NO-UNDO.
DEFINE VARIABLE must-add   AS LOG       NO-UNDO.
DEFINE VARIABLE ac-hdr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE rtab       AS RECID     EXTENT 24 NO-UNDO.
DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
DEFINE VARIABLE ok         AS LOG       FORMAT "Yes/No" NO-UNDO.
DEFINE VARIABLE fr-header AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcEUName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEUConsName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFSName      AS CHARACTER NO-UNDO.
DEFINE BUFFER bfCCRule   FOR CCRule.

FORM
    CCRule.BillCode FORMAT "X(20)"
    BillItem.BIName COLUMN-LABEL "BI Name" FORMAT "x(16)"
    CCRule.Category 
    CCRule.ValidTo   
    CCRule.CostCentre COLUMN-LABEL "CostCentre"
    CCRule.AccNum
    WITH WIDTH 80 OVERLAY ROW 1 SCROLL 1 15 DOWN COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
    " ACCOUNTING RULES " + string(TODAY,"99-99-99") + " "
    FRAME sel.

FORM
    CCRule.CCRuleID  COLON 30 
    CCRule.Brand     COLON 30 
    CCRule.BillCode    COLON 30 FORMAT "X(32)" SKIP
    BillItem.BIName NO-LABELS COLON 30  
    CCRule.Category    COLON 30 FORMAT "X(8)" 
    CustCat.CatName NO-LABELS 
    CCRule.ValidFrom  COLON 30 
    CCRule.ValidTo    COLON 30
    CCRule.CostCentre COLON 30 
    CostCentre.CcName NO-LABELS
    CCRule.AccNum  COLON 30 
    Account.ACCName NO-LABELS 
    CCRule.EUAccNum  COLON 30 
    lcEUName NO-LABELS FORMAT "X(30)" 
    CCRule.EUConAccNum  COLON 30 
    lcEUConsName NO-LABELS FORMAT "X(30)" 
    CCRule.FSAccNum  COLON 30 
    lcFSName NO-LABELS FORMAT "X(30)"
    CCRule.ReportingID COLON 30     
    WITH  OVERLAY ROW 2 CENTERED COLOR VALUE(Syst.Var:cfc) TITLE COLOR VALUE(Syst.Var:ctc)
    fr-header WITH SIDE-LABELS FRAME lis.
    
{Func/brand.i}   

FORM /* seek CCRule  by  CCRule */
    lcBillCode
    HELP "Enter billing item "
    WITH ROW 4 COL 2 TITLE COLOR value(Syst.Var:ctc) " FIND Billing Item"
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

FORM /* seek CCRule  by CustCat */
    lcCategory
    HELP "Enter category"
    WITH ROW 4 COL 2 TITLE COLOR value(Syst.Var:ctc) " FIND Category "
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.               

IF icBillCode > "" THEN ASSIGN 
   order = 1
   maxorder = 1
   FrmRow = 3
   FrmDown = 10.
ELSE IF icCategory > "" THEN ASSIGN
   order = 2
   maxorder = 1
   FrmRow = 3
   FrmDown = 10.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. 
ASSIGN 
    Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

RUN local-find-first. 
IF AVAILABLE CCRule THEN ASSIGN
   memory       = RECID(CCRule)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

    IF must-add THEN 
    DO:  /* CCRULE -ADD  */
        ASSIGN 
            Syst.Var:cfc = "lis" 
            ufkey        = TRUE 
            fr-header    = " ADD" 
            must-add     = FALSE.
        RUN Syst/ufcolor.p.
        
      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
          PAUSE 0 NO-MESSAGE.
          Syst.Var:ehto = 9.
          RUN Syst/ufkey.p.
        
        REPEAT TRANSACTION WITH FRAME lis
        ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW:
         
           CLEAR FRAME lis NO-PAUSE.
           
           DISPLAY 
              icBillCode @ CCRule.BillCode
              icCategory @ CCRule.Category WITH FRAME lis.
              
           PROMPT-FOR 
              CCRule.BillCode WHEN icBillCode = ""
              CCRule.Category WHEN icCategory = ""
           WITH FRAME lis EDITING:
           
              READKEY.
              
              IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME lis:
                
                 PAUSE 0.
                    
                 IF FRAME-FIELD = "BillCode" THEN DO:
                 
                    IF INPUT CCRule.BillCode NE "" THEN DO:
                       FIND FIRST BillItem WHERE 
                            BillItem.Brand = Syst.Var:gcBrand AND
                            BillItem.BIllCode = INPUT CCRule.BillCode
                       NO-LOCK NO-ERROR.
                       IF NOT AVAILABLE BillItem THEN DO:
                          BELL.
                          MESSAGE "Unknown billing item".
                          NEXT.
                       END.
                       DISPLAY BillItem.BIName WITH FRAME lis.
                    END.
                 END.
 
                 ELSE IF FRAME-FIELD = "Category" THEN DO:
                 
                    IF INPUT CCRule.Category NE "" THEN DO:
                        
                       IF CAN-FIND(FIRST bfCCRule WHERE bfCCRule.Brand    = Syst.Var:gcBrand
                                                    AND bfCCRule.Category = TRIM(INPUT CCRule.Category)
                                                    AND bfCCRule.BillCode = icBillCode
                                                    AND bfCCRule.CLIType  = ""
                                                    AND bfCCRule.ValidTo  >= TODAY)
                       THEN DO:
                           BELL.
                           MESSAGE "Account Rule already exist with same Category and ValidTo dates.".
                           NEXT.                         
                       END.
                        
                       FIND FIRST CustCat WHERE 
                          CustCat.Brand = Syst.Var:gcBrand AND
                          CustCat.Category = INPUT CCRule.Category
                       NO-LOCK NO-ERROR.
                       IF NOT AVAILABLE CustCat AND TRIM(INPUT CCRule.Category) <> "*" THEN DO:
                          BELL.
                          MESSAGE "Unknown category".
                          NEXT.
                       END.
                       ELSE IF AVAILABLE CustCat THEN 
                           DISPLAY CustCat.CatName WITH FRAME lis.
                    END.
                 END.
                     
              END.
                 
              APPLY LASTKEY.
                      
           END. 

           IF INPUT FRAME lis CCRule.BillCode = "" OR
              INPUT FRAME lis CCRule.Category = ""
           THEN LEAVE add-row.
        
           CREATE CCRule.
           ASSIGN
              CCRule.Brand    = Syst.Var:gcBrand
              CCRule.CCRuleID = NEXT-VALUE(CCRuleSeq)
              CCRule.BillCode = INPUT FRAME lis CCRule.BillCode
              CCRule.Category = INPUT FRAME lis CCRule.Category
              CCRule.ValidFrom = TODAY.
              CCRule.ValidTo   = 12/31/2049 .
            
           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCCRule).

           ASSIGN
           memory = RECID(CCRule)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CCRule WHERE CCRule.Brand = Syst.Var:gcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CCRule THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CCRule WHERE RECID(CCRule) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CCRule THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = RECID(CCRule).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        UP FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

       IF ufkey THEN 
       DO:
           ASSIGN
               Syst.Var:ufk    = 0 
               Syst.Var:ufk[1] = 703  WHEN icBillCode = ""
               Syst.Var:ufk[2] = 1709 WHEN icCategory = ""
               Syst.Var:ufk[5] = 5  
               Syst.Var:ufk[6] = 4 
               Syst.Var:ufk[8] = 8 
               Syst.Var:ufk[9] = 1
               Syst.Var:ehto   = 3 
               ufkey           = FALSE.
           RUN Syst/ufkey.p.
       END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CCRule.BillCode ;(Syst/uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY value(Syst.Var:ccc) CCRule.BillCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CCRule.Category ;(Syst/uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY value(Syst.Var:ccc) CCRule.Category WITH FRAME sel.
      END.

       Syst.Var:nap = KEYLABEL(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.
                                       
      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 AND maxorder > 1 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 AND maxorder > 1 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND CCRule WHERE RECID(CCRule) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CCRule THEN
              ASSIGN FIRSTrow = i memory = RECID(CCRule).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious row */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE CCRule THEN DO:
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
                rtab[1] = RECID(CCRule)
                memory  = rtab[1].
           END.
        END.
        ELSE UP 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE CCRule THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = RECID(CCRule).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND CCRule WHERE RECID(CCRule) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CCRule THEN DO:
           memory = RECID(CCRule).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CCRule THEN memory = RECID(CCRule).
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND CCRule WHERE RECID(CCRule) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND Syst.Var:ufk[1] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
          Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
          Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       lcBillCode = "".
       UPDATE lcBillCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcBillCode > "" THEN DO:

          IF icCategory > "" THEN 
          FIND FIRST CCRule WHERE 
             CCRule.Brand = Syst.Var:gcBrand AND
             CCRule.Category = icCategory AND
             CCRule.BillCode >= lcBillCode NO-LOCK NO-ERROR.
          ELSE FIND FIRST CCRule WHERE 
             CCRule.Brand = Syst.Var:gcBrand AND
             CCRule.BillCode >= lcBillCode NO-LOCK NO-ERROR.
          IF NOT AVAILABLE CCRule THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some CCRule/CCRule was found */
          ASSIGN order = 1 WHEN icCategory = ""
                 memory = RECID(CCRule) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 AND Syst.Var:ufk[2] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

          Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
          Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f2.
       lcCategory = "".
       UPDATE lcCategory WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF lcCategory > "" THEN DO:
          IF icBillCode > "" THEN 
          FIND FIRST CCRule WHERE 
             CCRule.Brand = Syst.Var:gcBrand AND
             CCRule.BillCode = icBillCode AND
             CCRule.Category >= lcCategory NO-LOCK NO-ERROR.
          ELSE FIND FIRST CCRule WHERE 
             CCRule.Brand = Syst.Var:gcBrand AND
             CCRule.Category >= lcCategory NO-LOCK NO-ERROR.
          IF NOT AVAILABLE CCRule THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some CCRule/CustCat was found */
          ASSIGN order = 2 WHEN icBillCode = ""
                 memory = RECID(CCRule) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i}.
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}.
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       CCRule.BillCode CCRule.Category CCRule.ValidTo CCRule.CostCentre.

       RUN local-find-NEXT.
       IF AVAILABLE CCRule THEN memory = RECID(CCRule).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CCRule THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = RECID(CCRule).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       CCRule.BillCode CCRule.Category CCRule.ValidTo CCRule.CostCentre.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCCRule).
           
           DELETE CCRule.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CCRule)
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       {Syst/uright2.i}
       RUN local-find-this(FALSE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
         Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCCRule).
       
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(LASTKEY) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCCRule).
       
       RUN local-disp-row.
       xrecid = RECID(CCRule).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = RECID(CCRule) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = RECID(CCRule) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.



PROCEDURE local-find-this:

    DEFINE INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND CCRule WHERE RECID(CCRule) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CCRule WHERE RECID(CCRule) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN DO:
          IF icBillCode > "" THEN 
             FIND FIRST CCRule WHERE 
                        CCRule.Brand = Syst.Var:gcBrand AND
                        CCRule.BillCode = icBillCode NO-LOCK NO-ERROR.
          ELSE FIND FIRST CCRule USE-INDEX BillCode NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN DO:
          IF icCategory > "" THEN 
             FIND FIRST CCRule WHERE
                        CCRule.Brand = Syst.Var:gcBrand AND
                        CCRule.Category = icCategory NO-LOCK NO-ERROR.
          ELSE FIND FIRST CCRule USE-INDEX Category NO-LOCK NO-ERROR.
       END.   
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN DO:
          IF icBillCode > "" THEN 
             FIND LAST CCRule WHERE 
                       CCRule.Brand = Syst.Var:gcBrand AND
                       CCRule.BillCode = icBillCode NO-LOCK NO-ERROR.
          ELSE FIND LAST CCRule USE-INDEX BillCode NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN DO:
          IF icCategory > "" THEN 
             FIND LAST CCRule WHERE
                       CCRule.Brand = Syst.Var:gcBrand AND
                       CCRule.Category = icCategory NO-LOCK NO-ERROR.
          ELSE FIND LAST CCRule USE-INDEX Category NO-LOCK NO-ERROR.
       END.   
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN DO:
          IF icBillCode > "" THEN 
             FIND NEXT CCRule WHERE 
                       CCRule.Brand = Syst.Var:gcBrand AND
                       CCRule.BillCode = icBillCode NO-LOCK NO-ERROR.
          ELSE FIND NEXT CCRule USE-INDEX BillCode NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN DO:
          IF icCategory > "" THEN 
             FIND NEXT CCRule WHERE
                       CCRule.Brand = Syst.Var:gcBrand AND
                       CCRule.Category = icCategory NO-LOCK NO-ERROR.
          ELSE FIND NEXT CCRule USE-INDEX Category NO-LOCK NO-ERROR.
       END.   
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN DO:
          IF icBillCode > "" THEN 
             FIND PREV CCRule WHERE 
                       CCRule.Brand = Syst.Var:gcBrand AND
                       CCRule.BillCode = icBillCode NO-LOCK NO-ERROR.
          ELSE FIND PREV CCRule USE-INDEX BillCode NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN DO:
          IF icCategory > "" THEN 
             FIND PREV CCRule WHERE
                       CCRule.Brand = Syst.Var:gcBrand AND
                       CCRule.Category = icCategory NO-LOCK NO-ERROR.
          ELSE FIND PREV CCRule USE-INDEX Category NO-LOCK NO-ERROR.
       END.   
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
          CCRule.BillCode
          BillItem.BIName WHEN AVAILABLE BillItem
          CCRule.Category
          CCRule.ValidTo
          CCRule.CostCentre
          CCRule.AccNum
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
    FIND FIRST BillItem WHERE
         BillItem.Brand = Syst.Var:gcBrand AND
         BillItem.BillCode = CCRule.BillCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-update-record:
  
   RuleDetail:
   REPEAT ON ENDKEY UNDO, LEAVE:
      
      RUN local-find-others.

      FIND FIRST CustCat WHERE
         CustCat.Brand = Syst.Var:gcBrand AND
         CustCat.Category = CCRule.Category NO-LOCK NO-ERROR.
      FIND FIRST CostCentre WHERE
         CostCentre.Brand = Syst.Var:gcBrand AND
         CostCentre.CostCentre = CCRule.CostCentre NO-LOCK NO-ERROR.

      ASSIGN
         lcEUName = ""
         lcEUConsName = ""
         lcFSName = "".
      FIND FIRST Account WHERE
         Account.Brand = Syst.Var:gcBrand AND
         Account.AccNum = CCRule.EUAccNum NO-LOCK NO-ERROR.
      IF AVAILABLE Account THEN lcEUName = Account.AccName.
      FIND FIRST Account WHERE
         Account.Brand = Syst.Var:gcBrand AND
         Account.AccNum = CCRule.EUConAccNum NO-LOCK NO-ERROR.
      IF AVAILABLE Account THEN lcEUConsName = Account.AccName.
      FIND FIRST Account WHERE
         Account.Brand = Syst.Var:gcBrand AND
         Account.AccNum = CCRule.FSAccNum NO-LOCK NO-ERROR.
      IF AVAILABLE Account THEN lcFSName = Account.AccName.
      
      FIND FIRST Account WHERE
         Account.Brand = Syst.Var:gcBrand AND
         Account.AccNum = CCRule.AccNum NO-LOCK NO-ERROR.
      
      CLEAR FRAME lis NO-PAUSE.
      
      DISPLAY
         CCRule.CCRuleID
         CCRule.Brand 
         CCRule.BillCode
         BillItem.BIName WHEN AVAILABLE BillItem
         CCRule.Category
         CustCat.CatName WHEN AVAILABLE CustCat
         CCRule.ValidFrom
         CCRule.ValidTo 
         CCRule.CostCentre
         CostCentre.CCName WHEN AVAILABLE CostCentre
         CCRule.AccNum
         Account.ACCName WHEN AVAILABLE Account
         CCRule.EUAccNum lcEUName
         CCRule.EUConAccNum lcEUConsName
         CCRule.FSAccNum lcFSName
         CCRule.ReportingID
      WITH FRAME lis.

       IF NEW CCRule THEN Syst.Var:toimi = 1.
       ELSE 
       DO: 
           ASSIGN 
               Syst.Var:ehto   = 0
               Syst.Var:ufk    = 0
               Syst.Var:ufk[1] = 7 WHEN Syst.Var:gcHelpParam = ""
               Syst.Var:ufk[8] = 8.
           RUN Syst/ufkey.p.
       END.
      
      IF Syst.Var:toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE RuleDetail:

         FIND CURRENT CCRule EXCLUSIVE-LOCK.
            
          Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
 
         UPDATE
            CCRule.ValidFrom
            CCRule.ValidTo
            CCRule.CostCentre
            CCRule.AccNum
            CCRule.EUAccNum
            CCRule.EUConAccNum
            CCRule.FSAccNum
            CCRule.ReportingID
         WITH FRAME lis EDITING:
      
             READKEY.
             
             IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "CostCentre" THEN DO:
                   FIND FIRST CostCentre WHERE 
                      CostCentre.Brand = Syst.Var:gcBrand AND
                      CostCentre.CostCentre = INPUT FRAME lis 
                          CCRule.CostCentre NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE CostCentre THEN DO:
                      BELL.
                      MESSAGE "Unknown Cost Centre".
                      NEXT.
                   END.
                   DISPLAY CostCentre.CcName.
                END.

                ELSE IF FRAME-FIELD = "AccNum" THEN DO:
                   FIND FIRST Account WHERE
                      Account.Brand = Syst.Var:gcBrand AND
                      Account.AccNum = INPUT FRAME lis CCRule.AccNum 
                      NO-LOCK NO-ERROR.
                   IF INPUT FRAME lis CCRule.AccNum = 0 THEN 
                      DISPLAY "" @ Account.ACCName.
                   ELSE IF NOT AVAILABLE Account THEN DO:
                      BELL.
                      MESSAGE "Unknown account".
                      NEXT.
                   END.
                   ELSE DISPLAY Account.ACCName.
                END.

                ELSE IF FRAME-FIELD = "EUAccNum" THEN DO:
                   FIND FIRST Account WHERE
                      Account.Brand = Syst.Var:gcBrand AND
                      Account.AccNum = INPUT FRAME lis CCRule.EuAccNum 
                      NO-LOCK NO-ERROR.
                   IF INPUT FRAME lis CCRule.EUAccNum = 0 THEN 
                      DISPLAY "" @ lcEUName.
                   ELSE IF NOT AVAILABLE Account THEN DO:
                      BELL.
                      MESSAGE "Unknown account".
                      NEXT.
                   END.
                   ELSE DISPLAY Account.ACCName @ lcEUName.
                END.
                
                ELSE IF FRAME-FIELD = "EUConsAccNum" THEN DO:
                   FIND FIRST Account WHERE
                      Account.Brand = Syst.Var:gcBrand AND
                      Account.AccNum = INPUT FRAME lis CCRule.EUConAccNum 
                      NO-LOCK NO-ERROR.
                   IF INPUT FRAME lis CCRule.EUConAccNum = 0 THEN 
                      DISPLAY "" @ lcEUConsName.
                   ELSE IF NOT AVAILABLE Account THEN DO:
                      BELL.
                      MESSAGE "Unknown account".
                      NEXT.
                   END.
                   ELSE DISPLAY Account.ACCName @ lcEUConsName.
                END.
                
                ELSE IF FRAME-FIELD = "FSAccNum" THEN DO:
                   FIND FIRST Account WHERE
                      Account.Brand = Syst.Var:gcBrand AND
                      Account.AccNum = INPUT FRAME lis CCRule.FSAccNum 
                      NO-LOCK NO-ERROR.
                   IF INPUT FRAME lis CCRule.FSAccNum = 0 THEN 
                      DISPLAY "" @ lcFSName.
                   ELSE IF NOT AVAILABLE Account THEN DO:
                      BELL.
                      MESSAGE "Unknown account".
                      NEXT.
                   END.
                   ELSE DISPLAY Account.ACCName @ lcFSName.
                END.
                 
             END.
             APPLY LASTKEY.
         END. /* EDITING */
   
         IF NEW CCRule THEN LEAVE RuleDetail.
         ELSE LEAVE.
      END.
      ELSE IF Syst.Var:toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.
