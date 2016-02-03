/* ----------------------------------------------------------------------
  MODULE .......: CoBasis
  TASK .........: UPDATEs table CoBasis
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 11.11.03
  CHANGED ......: 01.06.04/aam percents and fixed amounts moved from CoTarg
                  30.08.04/aam brand
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CoBasis'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCoBasis AS HANDLE NO-UNDO.
   lhCoBasis = BUFFER CoBasis:HANDLE.
   RUN StarEventInitialize(lhCoBasis).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhCoBasis).
   END.

END.

DEF INPUT PARAMETER iiRuleID AS INT NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcTarg       LIKE CoBasis.BillCode      NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
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
DEF VAR lcTitle      AS CHAR                   NO-UNDO. 
DEF VAR lcCCNName    AS CHAR                   NO-UNDO. 
DEF VAR lcBIName     AS CHAR                   NO-UNDO. 

form
    CoBasis.BillCode
    lcBIName     COLUMN-LABEL "Name" FORMAT "X(17)"
    CoBasis.CCN                      FORMAT "->>>"
    lcCCNName    COLUMN-LABEL "Name" FORMAT "X(16)"
    CoBasis.CoAmt 
    CoBasis.CoPerc
    
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    lcTitle
    FRAME sel.

form
    CoBasis.CCN        COLON 20  FORMAT "->>>" 
       lcCCNName NO-LABEL FORMAT "X(30)" 
    CoBasis.BillCode   COLON 20
       lcBIName  NO-LABEL FORMAT "X(30)"
    CoBasis.SubsQty    COLON 20 
    CoBasis.CoPerc     COLON 20
    CoBasis.CoAmt      COLON 20 
    CoBasis.CommLimit  COLON 20
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  target */
    lcTarg
    HELP "Enter target type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND target type "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FIND CoRule WHERE 
     CoRule.Brand    = gcBrand  AND
     CoRule.CoRuleId = iiRuleID NO-LOCK.
     
ASSIGN lcTitle = " COMM. BASIS FOR: " +
                 STRING(CoRule.CustNum) + "/" +
                 CoRule.BillCode         + "/" +
                 STRING(CoRule.CCN)    + "/" +
                 STRING(CoRule.CoFrom,"99.99.99") + " ". 

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By BillCode ,    ,   , By 4".

RUN local-find-first.

IF AVAILABLE CoBasis THEN ASSIGN
   Memory       = recid(CoBasis)
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a CoBasis  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR CoBasis.CCN
                      CoBasis.BillCode
           EDITING:
              READKEY.

              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.

                 IF FRAME-FIELD = "BillCode" THEN DO:
                    IF INPUT CoBasis.BillCode = "" OR
                       INPUT CoBasis.CCN = -1 
                    THEN DISPLAY "" @ lcBIName WITH FRAME lis.
                    
                    ELSE DO:
                       FIND BillItem WHERE 
                            BillItem.Brand    = gcBrand  AND
                            BillItem.BillCode = INPUT CoBasis.BillCode
                       NO-LOCK NO-ERROR.
                       IF NOT AVAILABLE BillItem THEN DO:
                          BELL.
                          MESSAGE "Unknown billing item".
                          NEXT.
                       END.
                       
                       DISPLAY BillItem.BIName @ lcBIName WITH FRAME lis.
                    END.
 
                 END.
                 
                 ELSE IF FRAME-FIELD = "CCN" THEN DO:
                    IF INPUT CoBasis.CCN = ""
                    THEN DISPLAY "" @ lcCCNName WITH FRAME lis.

                    ELSE IF INPUT CoBasis.CCN = -1 
                    THEN DISPLAY "Previous Operator" @ lcCCNName 
                         WITH FRAME lis.
                    
                    ELSE DO:
                       FIND CCN WHERE 
                            CCN.Brand = gcBrand  AND
                            CCN.CCN   = INPUT CoBasis.CCN
                       NO-LOCK NO-ERROR.
                       IF NOT AVAILABLE CCN THEN DO:
                          BELL.
                          MESSAGE "Unknown CCN".
                          NEXT.
                       END.
                       
                       DISPLAY CCN.CCNName @ lcCCNName WITH FRAME lis.
                    END.
                        
                 END.

              END.
              
              APPLY LASTKEY.
           END.               

           IF INPUT FRAME lis CoBasis.BillCode = "" AND
              INPUT FRAME lis CoBasis.CCN = 0
           THEN LEAVE add-row.

           IF CAN-FIND(
              FIRST CoBasis WHERE 
                    CoBasis.Brand    = gcBrand  AND
                    CoBasis.CoRuleid = iiRuleId AND  
                    CoBasis.BillCode = INPUT FRAME lis CoBasis.BillCode AND
                    CoBasis.CCN      = INPUT FRAME lis CoBasis.CCN)
           THEN DO:
              MESSAGE 
              "Commission basis for" 
              INPUT FRAME lis CoBasis.BillCode "/"
              INPUT FRAME lis CoBasis.CCN 
              "already exists !"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           CREATE CoBasis.
           ASSIGN
           CoBasis.Brand    = gcBrand  
           CoBasis.CoRuleID = iiRuleID 
           CoBasis.BillCode = INPUT FRAME lis CoBasis.BillCode
           CoBasis.CCN      = INPUT FRAME lis CoBasis.CCN.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCoBasis).

           ASSIGN
           Memory = recid(CoBasis)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CoBasis
       NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CoBasis THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CoBasis WHERE recid(CoBasis) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CoBasis THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CoBasis).
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
        ufk[1]= 0  ufk[2]= 0  ufk[3]= 0  ufk[4]= 0
        ufk[5]= 5 
        ufk[6]= 4
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CoBasis.BillCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CoBasis.BillCode WITH FRAME sel.
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
        FIND CoBasis WHERE recid(CoBasis) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CoBasis THEN
              ASSIGN FIRSTrow = i Memory = recid(CoBasis).
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
           IF NOT AVAILABLE CoBasis THEN DO:
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
                rtab[1] = recid(CoBasis)
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
           IF NOT AVAILABLE CoBasis THEN DO:
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
              rtab[FRAME-DOWN] = recid(CoBasis).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CoBasis WHERE recid(CoBasis) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CoBasis THEN DO:
           Memory = recid(CoBasis).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CoBasis THEN Memory = recid(CoBasis).
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
           FIND CoBasis WHERE recid(CoBasis) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */


     ELSE IF LOOKUP(nap,"5,f5") > 0 
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       CoBasis.BillCode CoBasis.CCN .

       RUN local-find-NEXT.
       IF AVAILABLE CoBasis THEN Memory = recid(CoBasis).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CoBasis THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CoBasis).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CoBasis.BillCode CoBasis.CCN .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCoBasis).

           DELETE CoBasis.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CoBasis WHERE 
                                 CoBasis.Brand    = gcBrand  AND
                                 CoBasis.CoRuleId = iiRuleID) 
           THEN DO:
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
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCoBasis).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CoBasis.BillCode.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCoBasis).

       RUN local-disp-row.
       xrecid = recid(CoBasis).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CoBasis) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CoBasis) must-print = TRUE.
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
      FIND CoBasis WHERE recid(CoBasis) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CoBasis WHERE recid(CoBasis) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CoBasis
        WHERE CoBasis.Brand    = gcBrand  AND
              CoBasis.CoRuleId = iiRuleID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CoBasis
        WHERE CoBasis.Brand    = gcBrand  AND
              CoBasis.CoRuleId = iiRuleID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CoBasis
        WHERE CoBasis.Brand    = gcBrand  AND
              CoBasis.CoRuleId = iiRuleID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CoBasis
        WHERE CoBasis.Brand    = gcBrand  AND
              CoBasis.CoRuleId = iiRuleID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       CoBasis.BillCode 
       lcBIName
       CoBasis.CCN
       lcCCNName 
       CoBasis.CoAmt
       CoBasis.CoPerc
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   ASSIGN lcBIName  = ""
          lcCCNName = "". 

   IF CoBasis.BillCode > "" AND CoBasis.CCN >= 0 THEN DO:
      FIND BillItem WHERE 
           BillItem.Brand    = gcBrand  AND
           BillItem.BillCode = CoBasis.BillCode 
      NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem THEN lcBIName = BillItem.BIName.
   END.

   IF CoBasis.CCN > 0 THEN DO:
      FIND CCN WHERE 
           CCN.Brand = gcBrand  AND
           CCN.CCN   = CoBasis.CCN NO-LOCK NO-ERROR.
      IF AVAILABLE CCN THEN lcCCNName = CCN.CCNName.
   END.
   ELSE IF CoBasis.CCN = -1 THEN lcCCNName = "Previous Operator".
   

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      DISP CoBasis.BillCode
           CoBasis.CCN
           lcCCNName 
      WITH FRAME lis.
      
      IF lcRight = "RW" THEN DO:
      
         ehto = 9. RUN ufkey.
       
         UPDATE
         CoBasis.SubsQty WHEN NOT NEW CoBasis
         CoBasis.CoPerc  WHEN CoRule.BasisType NE 3
         CoBasis.CoAmt   
         CoBasis.CommLimit
         WITH FRAME lis.
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
           
      LEAVE.
   END.
   
END PROCEDURE.

