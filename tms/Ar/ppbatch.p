/* ----------------------------------------------------------------------
  MODULE .......: PPBatch
  TASK .........: UPDATEs table PPBatch
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.03.04
  CHANGED ......: 10.05.04/aam move directly to invoices (ppinv),
                               directly to insert according to ilAutoMode,
                               automatic dividing denied if ilAutoMode
                  15.11.04/aam fPBStatus returns false if unknown value
                  15.03.06/aam ask the nbr of batches from user,
                               new input parameters
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/finvbal.i}
{Func/fppbatch.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PPBatch'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPPBatch AS HANDLE NO-UNDO.
   lhPPBatch = BUFFER PPBatch:HANDLE.
   RUN StarEventInitialize(lhPPBatch).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhPPBatch).
   END.

END.
                                    
DEF INPUT  PARAMETER iiPlanID   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiBatchQty AS INT  NO-UNDO.
DEF INPUT  PARAMETER idtFirst   AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtLast    AS DATE NO-UNDO.
DEF OUTPUT PARAMETER ol2Inv     AS LOG  NO-UNDO. 

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 7.
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

DEF VAR lcStatus     AS CHAR                   NO-UNDO. 
DEF VAR lcStatusLst  AS CHAR                   NO-UNDO. 
DEF VAR lcCode       AS CHAR                   NO-UNDO. 
DEF VAR liCnt        AS INT                    NO-UNDO.
DEF VAR ldTotAmt     AS DEC                    NO-UNDO. 
DEF VAR lcError      AS CHAR                   NO-UNDO. 
DEF VAR ldBatchTot   AS DEC                    NO-UNDO.
DEF VAR llChanged    AS LOG                    NO-UNDO. 
DEF VAR lcInfo       AS CHAR                   NO-UNDO. 
DEF VAR ldPaid       AS DEC                    NO-UNDO. 
DEF VAR llDivBatches AS LOG                    NO-UNDO.
DEF VAR liBatch      AS INT                    NO-UNDO. 
DEF VAR ldtFromDate  AS DATE                   NO-UNDO.
DEF VAR ldtToDate    AS DATE                   NO-UNDO.

DEF BUFFER bPPBatch FOR PPBatch.
DEF BUFFER bPPlan   FOR PaymPlan.

DEF TEMP-TABLE ttBatch NO-UNDO
   FIELD PPBatch AS INT.
   
form
    PPBatch.PPBatch 
    PPBatch.DueDate
    PPBatch.Amount
    PPBatch.RefNum   FORMAT "X(18)"  COLUMN-LABEL "Reference"
    PPBatch.PaidAmt   
    lcStatus         COLUMN-LABEL "Status"     FORMAT "X(11)"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    lcTitle
    FRAME sel.

FORM
    PaymPlan.Amount TO 40
       LABEL "Total Sum of Invoices"
       FORMAT "->,>>>,>>9.99"
       SKIP
    ldBatchTot TO 40
       LABEL "Divided to Batches"
       FORMAT "->,>>>,>>9.99"
    ldPaid     TO 40 
       LABEL "Paid"
       FORMAT "->,>>>,>>9.99"
    WITH OVERLAY ROW 14 WIDTH 42 CENTERED SIDE-LABELS FRAME fTotals.
     
form
    PPBatch.PPBatch  COLON 20
    PPBatch.DueDate  COLON 20
       VALIDATE(INPUT PPBatch.DueDate NE ?,
                "Due date is mandatory")
    PPBatch.Amount   COLON 20
    PPBatch.PaidAmt  COLON 20 
    PPBatch.PBStatus COLON 20
       lcStatus 
          NO-LABEL 
          FORMAT "X(30)"
    PPBatch.RefNum   COLON 20
WITH  OVERLAY ROW 6 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM
   PaymPlan.Amount AT 2
      LABEL "Debt ............"
      SKIP
   liBatch AT 2 
      LABEL "Number Of Batches"
      FORMAT ">9"
      HELP "How many batches debt will be divided into"
      SKIP
   ldtFromDate AT 2 
      LABEL "First Due Date .."
      FORMAT "99-99-9999"
      HELP "Due date for the 1. batch"
      SKIP
   ldtToDate AT 2 
      LABEL "Last Due Date ..."
      FORMAT "99-99-9999"
      HELP "Due date for the last batch"
      SKIP
   WITH SIDE-LABELS OVERLAY ROW 7 CENTERED TITLE " CREATE BATCHES "   
      FRAME fBatches. 

FUNCTION fPPTotAmount RETURNS LOGICAL.

END FUNCTION.

FUNCTION fDispTotals RETURNS LOGICAL.
   FOR EACH bPPBatch OF PaymPlan NO-LOCK:
      ACCUMULATE bPPBatch.Amount  (TOTAL)
                 bPPBatch.PaidAmt (TOTAL).
   END.
   ASSIGN ldBatchTot = (ACCUM TOTAL bPPBatch.Amount)
          ldPaid     = (ACCUM TOTAL bPPBatch.PaidAmt).
   
   PAUSE 0.
   DISPLAY ldBatchTot PaymPlan.Amount ldPaid WITH FRAME fTotals.
    
END FUNCTION.

FUNCTION fPPUndivided RETURNS DECIMAL.

   DEF VAR ldUndivided AS DEC NO-UNDO.
   
   ldUndivided = 0.
   FOR EACH PPInv OF PaymPlan NO-LOCK:
      ldUndivided = ldUndivided + PPInv.Amount.
   END.   

   FOR EACH bPPBatch OF PaymPlan NO-LOCK:
   
      ldUndivided = ldUndivided - bPPBatch.Amount.
   END.

   RETURN ldUndivided.
   
END.

FUNCTION fPBStatus RETURNS LOGICAL
   (iiType AS INT,
    ilDisp AS LOG).

   IF iiType >= 0 AND iiType <= 5 
   THEN lcStatus = ENTRY(iiType + 1,lcStatusLst).
   ELSE lcStatus = "".
   
   IF ilDisp THEN 
   DISPLAY lcStatus WITH FRAME lis.
   
   RETURN (lcStatus > "").
   
END.


DO i = 0 TO 5:
   lcStatusLst = lcStatusLst + 
                DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "PPBatch","PBStatus",STRING(i)) + ",". 
END.

FIND PaymPlan WHERE 
     PaymPlan.PPlanID = iiPlanID NO-LOCK NO-ERROR.
IF NOT AVAILABLE PaymPlan THEN RETURN.
     
lcTitle = " BATCHES OF PLAN: " +
          STRING(PaymPlan.CustNum) + "/" +
          STRING(PaymPlan.PPDate,"99.99.9999") + " ".

/* current batches -> check later if new ones were made */
llDivBatches = (PaymPlan.PPStatus < 3 OR PaymPlan.PPStatus = 7).
FOR EACH PPBatch OF PaymPlan NO-LOCK:
   CREATE ttBatch.
   ASSIGN ttBatch.PPBatch = PPBatch.PPBatch
          llDivBatches    = FALSE.
END.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

ASSIGN orders    = "  By Batch ,    ,   "
       llChanged = FALSE
       ol2Inv    = FALSE.

RUN local-find-first.

IF AVAILABLE PPBatch THEN ASSIGN
   Memory       = recid(PPBatch)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   /* if automatic mode -> directly to insert */
   must-add     = FALSE.

fDispTotals().

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   IF must-add THEN DO:  /* Add a PPBatch  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        liBatch = 1.
        FIND LAST PPBatch OF PaymPlan NO-LOCK NO-ERROR.
        IF AVAILABLE PPBatch THEN liBatch = PPBatch.PPBatch + 1.
        DISPLAY liBatch @ PPBatch.PPBatch WITH FRAME lis.
        
        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           IF CAN-FIND(
              FIRST PPBatch WHERE 
                    PPBatch.PPlanID = iiPlanID AND  
                    PPBatch.PPBatch = INPUT FRAME lis PPBatch.PPBatch)
           THEN DO:
              MESSAGE 
              "Batch" 
              INPUT FRAME lis PPBatch.PPBatch
              "already exists for this plan !"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           CREATE PPBatch.
           ASSIGN
           PPBatch.PPlanID  = iiPlanID 
           PPBatch.PPBatch  = INPUT FRAME lis PPBatch.PPBatch
           PPBatch.BatchFee = IF liFeeQty >= PPBatch.PPBatch
                              THEN DECIMAL(ENTRY(PPBatch.PPBatch,
                                                 lcBatchFee,"/"))
                              ELSE 0.
           PPBatch.Amount   = fPPUndivided().

           IF PaymPlan.PPType = 3 THEN    
              PPBatch.RefNum   = fFormRefNum(PaymPlan.CustNum,
                                             PaymPlan.PPlanID,
                                             -1 * PPBatch.PPBatch).
           ELSE 
              PPBatch.RefNum   = fFormRefNum(PaymPlan.CustNum,
                                             0,
                                             0).
                                            

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPPBatch).

           fDispTotals().
           
           IF ldBatchTot >= PaymPlan.Amount THEN LEAVE ADD-ROW. 
            
           ASSIGN
           Memory = recid(PPBatch)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE PPBatch THEN LEAVE LOOP.
      ASSIGN Memory = recid(PPBatch)
             xrecid = Memory.  
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND PPBatch WHERE recid(PPBatch) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PPBatch THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PPBatch).
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

      IF ufkey AND NOT llDivBatches THEN DO:
         ASSIGN
         ufk   = 0
         ufk[3]= 1777
         ufk[2]= (IF lcRight = "RW" AND 
                  LOOKUP(STRING(PaymPlan.PPStatus),"1,7") > 0 THEN 1779 ELSE 0)
         ufk[5]= (IF lcRight = "RW" AND 
                  LOOKUP(STRING(PaymPlan.PPStatus),"1,7") > 0 THEN 5 ELSE 0)
         ufk[6]= (IF lcRight = "RW" AND
                  LOOKUP(STRING(PaymPlan.PPStatus),"1,7") > 0 THEN 4 ELSE 0)
         ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         
         IF CAN-FIND(FIRST PPBatch OF PaymPlan) THEN ufk[2] = 0.
         
         RUN Syst/ufkey.
      END.
      
      HIDE MESSAGE NO-PAUSE.
      
      IF NOT llDivBatches THEN DO:
         IF order = 1 THEN DO:
            CHOOSE ROW PPBatch.PPBatch {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
            COLOR DISPLAY VALUE(ccc) PPBatch.PPBatch WITH FRAME sel.
         END.
      
         nap = keylabel(LASTKEY).
      END.
      ELSE ASSIGN nap          = "f2"
                  llDivBatches = FALSE.
      

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"3,f3,2,f2,5,f5,8,f8") = 0 THEN DO:
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
        FIND PPBatch WHERE recid(PPBatch) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PPBatch THEN
              ASSIGN FIRSTrow = i Memory = recid(PPBatch).
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
           IF NOT AVAILABLE PPBatch THEN DO:
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
                rtab[1] = recid(PPBatch)
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
           IF NOT AVAILABLE PPBatch THEN DO:
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
              rtab[FRAME-DOWN] = recid(PPBatch).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PPBatch WHERE recid(PPBatch) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PPBatch THEN DO:
           Memory = recid(PPBatch).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PPBatch THEN Memory = recid(PPBatch).
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
           FIND PPBatch WHERE recid(PPBatch) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* create batches automatically according to chosen invoices */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 THEN DO:  /* add */
        {Syst/uright2.i}
        
        IF iiBatchQty = 0 THEN DO:
           /* ask nbr of batches from user */
           PAUSE 0.
           DISPLAY PaymPlan.Amount WITH FRAME fBatches.
           ASSIGN liBatch     = 0  
                  ldtFromDate = ?
                  ldtToDate   = ?
                  ehto        = 9
                  ufkey       = TRUE.
           RUN Syst/ufkey.
        
           REPEAT ON ENDKEY UNDO, LEAVE:
              UPDATE liBatch 
                     ldtFromDate
                     ldtToDate VALIDATE(INPUT ldtToDate >= INPUT ldtFromDate,
                                        "Date of last batch cannot be earlier")
              WITH FRAME fBatches.
              LEAVE.
           END.
           HIDE FRAME fBatches NO-PAUSE.
        END.
        ELSE ASSIGN liBatch     = iiBatchQty
                    ldtFromDate = idtFirst
                    ldtToDate   = idtLast.
        
        IF liBatch > 0 AND ldtFromDate NE ? AND ldtToDate NE ? THEN DO:
   
           IF NOT fPaymPlanBatches(liBatch,
                                   "",
                                   ldtFromDate,
                                   ldtToDate,
                                   OUTPUT lcError) 
           THEN MESSAGE "Dividing payment plan into batches failed;" SKIP
                        lcError
                VIEW-AS ALERT-BOX ERROR.

           ELSE DO:

              fPPTotAmount().
           
              fDispTotals().
            
              RUN local-find-first.
              ASSIGN must-print = TRUE
                     memory     = RECID(PPBatch).
              NEXT LOOP.
           END. 
        END.
           
     END. 
     
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

       /* has plan already been paid (deletion allowed only for plans
          that are not yet in use but check anyway) */
       FOR FIRST Payment NO-LOCK WHERE
                 Payment.PPlanID  = iiPlanID:
          MESSAGE "A payment has already been posted to this payment plan." 
                  SKIP
                  "Deletion not allowed."
          VIEW-AS ALERT-BOX.
          NEXT.
       END. 

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       PPBatch.PPBatch PPBatch.DueDate PPBatch.Amount .

       RUN local-find-NEXT.
       IF AVAILABLE PPBatch THEN Memory = recid(PPBatch).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE PPBatch THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(PPBatch).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       PPBatch.PPBatch PPBatch.DueDate PPBatch.Amount .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPPBatch).

           DELETE PPBatch.

           fPPTotAmount().

           fDispTotals().
           
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(IF lcRight = "RW" THEN TRUE ELSE FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPPBatch).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PPBatch.PPBatch.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPPBatch).

       fDispTotals().
        
       RUN local-disp-row.
       xrecid = recid(PPBatch).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PPBatch) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PPBatch) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8,3,F3") > 0 THEN DO:

        IF ldBatchTot NE PaymPlan.Amount AND 
           (PaymPlan.PPStatus < 3 OR PaymPlan.PPStatus = 7)
        THEN DO:
     
           ok = FALSE.
           MESSAGE "Total sum of batches is not the same as total sum of" 
                   "invoices in payment plan. Do you still want to leave ?"
           VIEW-AS ALERT-BOX
           QUESTION
           BUTTONS YES-NO
           SET ok.
           
           IF NOT ok THEN NEXT.
        END.

        ASSIGN ok     = ?
               lcInfo = "".
               
        /* move directly to invoices */    
        IF LOOKUP(nap,"3,F3") > 0 THEN ol2Inv = TRUE.
        
        LEAVE LOOP.
     END.
     
  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
ASSIGN si-recid    = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND PPBatch WHERE recid(PPBatch) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PPBatch WHERE recid(PPBatch) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST PPBatch
        WHERE PPBatch.PPlanID = iiPlanID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST PPBatch
        WHERE PPBatch.PPlanID = iiPlanID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT PPBatch
        WHERE PPBatch.PPlanID = iiPlanID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV PPBatch
        WHERE PPBatch.PPlanID = iiPlanID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       PPBatch.PPBatch 
       PPBatch.DueDate
       PPBatch.Amount
       PPBatch.RefNum
       PPBatch.PaidAmt
       lcStatus
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

    ldTotAmt = PPBatch.Amount + PPBatch.BatchFee.
    
    fPBStatus(PPBatch.PBStatus,FALSE).
    
END PROCEDURE.

PROCEDURE local-UPDATE-record:


   REPEAT ON ENDKEY UNDO, LEAVE:
  
      RUN local-find-others.
      
      DISPLAY
       PPBatch.PPBatch 
       PPBatch.DueDate
       PPBatch.Amount
       PPBatch.PaidAmt
       PPBatch.PBStatus
       lcStatus
       PPBatch.RefNum
      WITH FRAME lis.
      
      IF lcRight = "RW"       AND 
         PPBatch.PBStatus = 0 AND 
         (PaymPlan.PPStatus < 4 OR PaymPlan.PPStatus = 7)
      THEN DO:
      
         ehto = 9. RUN Syst/ufkey.
       
         REPEAT:
         
         UPDATE
         PPBatch.DueDate  
         PPBatch.Amount   WHEN LOOKUP(STRING(PaymPlan.PPStatus),"1,7") > 0 
         WITH FRAME lis EDITING:
         
             READKEY.

             IF KEYLABEL(LASTKEY) = "F9" AND 
                FRAME-FIELD = "PBStatus" 
             THEN DO:
               
               RUN Help/h-tmscodes(INPUT "PPBatch",     /* TableName */
                                    "PBStatus",  /* FieldName */
                                    "AccRec", /* GroupCode */
                              OUTPUT lcCode).
              
               IF lcCode ne "" AND lcCode NE ? THEN DO:
                  fPBStatus(INTEGER(lcCode),TRUE).
                  DISPLAY INTEGER(lcCode) ;& PPBatch.PBStatus
                  WITH FRAME lis.   
               END.
                                      
               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.

         
             ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
             THEN DO WITH FRAME lis:
             
                PAUSE 0.
                
                IF FRAME-FIELD = "Amount"  THEN DO:

                   IF INPUT PPBatch.Amount = 0 THEN DO:
                      MESSAGE "Batch amount cannot be 0"
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                   END. 
                END.
                
                ELSE IF FRAME-FIELD = "DueDate" THEN DO:
                
                   IF PPBatch.PPBatch > 1 THEN DO:
                      FIND LAST bPPBatch OF PaymPlan WHERE
                                bPPBatch.PPBatch < PPBatch.PPBatch 
                      NO-LOCK NO-ERROR.
                      IF AVAILABLE bPPBatch AND 
                         bPPBatch.DueDate > INPUT PPBatch.DueDate
                      THEN DO:
                         BELL.
                         MESSAGE "Due date of previous batch is later"
                                 "than the given date for this batch".
                         NEXT.
                      END.
                   END.
                   
                   FIND FIRST bPPBatch OF PaymPlan WHERE
                              bPPBatch.PPBatch > PPBatch.PPBatch 
                   NO-LOCK NO-ERROR.
                   IF AVAILABLE bPPBatch AND 
                      bPPBatch.DueDate < INPUT PPBatch.DueDate
                   THEN DO:
                      BELL.
                      MESSAGE "Due date of next batch is earlier"
                              "than the given date for this batch".
                      NEXT.
                   END.
                        
                END.
                
                ELSE IF FRAME-FIELD = "PBStatus" THEN DO:
                   IF NOT fPBStatus(INPUT INPUT PPBatch.PBStatus,TRUE)
                   THEN DO:
                      BELL.
                      MESSAGE "Unknown status".
                      NEXT.
                   END. 
                END. 
                
             END.
             APPLY LASTKEY.
          END. /* EDITING */

          IF PPBatch.DueDate  ENTERED OR
             PPBatch.Amount   ENTERED 
          THEN DO:
             /* was old batch changed */
             llChanged = (CAN-FIND(FIRST ttBatch WHERE
                                         ttBatch.PPBatch = PPBatch.PPBatch)).
          END.
          
          IF fPPUndivided() < 0 THEN DO:
             MESSAGE "Total sum of batches exceeds total sum of invoices"
             VIEW-AS ALERT-BOX
             ERROR.
          END.
          ELSE LEAVE.
          
          END.
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
           
      LEAVE.
   END.
   
END PROCEDURE.

