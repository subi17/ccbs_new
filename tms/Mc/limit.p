/* ----------------------------------------------------------------------
  MODULE .......: tmcounter.p
  TASK .........: Lists tmcounters.
  APPLICATION ..: TMS
  AUTHOR .......: as
  CREATED ......: 05/2008
  CHANGED ......: 
  Version ......: xfera 
  ---------------------------------------------------------------------- */

{Syst/commali.i}.

DEFINE INPUT PARAM piCustNum   AS INT NO-UNDO. 
DEFINE INPUT PARAM piMsSeq     AS INT NO-UNDO. 
DEFINE INPUT PARAM piLimitType AS INT NO-UNDO. 
DEFINE INPUT PARAM piTMRuleSeq AS INT NO-UNDO. 

{Mc/lib/tokenlib.i}
{Func/dataformat.i}
{Func/flimitreq.i}
{Syst/tmsconst.i}

DEF NEW shared VAR siirto AS CHAR.

DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 4.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 12.
DEFINE VARIABLE order        AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE orders       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE maxOrder     AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE ufkey        AS LOGICAL                 NO-UNDO  init TRUE.
DEFINE VARIABLE delrow       AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE pr-order     AS INTEGER                 NO-UNDO.
DEFINE VARIABLE Memory       AS RECID                   NO-UNDO.
DEFINE VARIABLE RowNo        AS INTEGER                 NO-UNDO.
DEFINE VARIABLE must-print   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE must-add     AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE ac-hdr       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE rtab         AS RECID EXTENT 24         NO-UNDO.
DEFINE VARIABLE i            AS INTEGER                 NO-UNDO.
DEFINE VARIABLE j            AS INTEGER NO-UNDO. 
DEFINE VARIABLE ok           AS LOGICAL format "Yes/No" NO-UNDO.
DEFINE VARIABLE llAdmin      AS LOGICAL NO-UNDO INIT FALSE. 
DEFINE VARIABLE lcLimitId    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLimit      AS CHARACTER NO-UNDO FORMAT "x(10)".
DEFINE VARIABLE lcCustnum    AS INT NO-UNDO. 
DEFINE VARIABLE lcMsisdn     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcValueType  AS CHARACTER NO-UNDO format "x(12)". 
DEFINE VARIABLE lcValue      AS CHARACTER NO-UNDO format "x(7)". 
DEFINE VARIABLE lcHeader     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeValue     AS DECIMAL NO-UNDO.  
DEFINE VARIABLE lcSelected   AS CHARACTER NO-UNDO.  
DEFINE VARIABLE lcLisTitle   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLimitType  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ocResult     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcValueDesc  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCode       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCodeName   AS CHARACTER NO-UNDO. 
DEF VAR llTerminated AS LOG NO-UNDO.

IF getTMSRight("SYST,BILL") EQ "RW" THEN llAdmin = TRUE.
llTerminated = (piMsSeq > 0 AND
                NOT CAN-FIND(MobSub WHERE MobSub.MsSeq = piMsSeq)).

FIND TMSCodes WHERE
     TMSCodes.TableName = "Limit" AND
     TMSCodes.FieldName = "LimitType" AND
     TMSCodes.CodeValue = STRING(piLimitType) NO-LOCK NO-ERROR.
IF AVAIL TMSCodes THEN DO:
   lcHeader = "Limit - " + TMSCodes.CodeName.
   lcLimitType = TMSCodes.CodeName.
END.

FORM
    Limit.Custnum   COLUMN-LABEL "CustNbr" 
    Limit.MsSeq     COLUMN-LABEL "Subscr.ID"
    lcLimit         COLUMN-LABEL "Limit" FORMAT "x(15)"
    lcValue         COLUMN-LABEL "Value"
    lcValueType     COLUMN-LABEL "Unit/Name"
    Limit.FromDate  FORMAT       "99-99-9999" COLUMN-LABEL "From"
    Limit.ToDate    FORMAT       "99-99-9999" COLUMN-LABEL "To"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN SCROLL 1
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + lcHeader + " "
    FRAME sel.

form 
   "Limit Value .:" Limit.LimitAmt SKIP
   "Limit Type ..:" Limit.LimitType lcLimitType FORMAT "x(20)" SKIP
   "Valid From ..:" Limit.FromDate FORMAT "99-99-9999" SKIP
   "Valid To ....:" Limit.ToDate FORMAT "99-99-9999" SKIP

WITH OVERLAY ROW 6 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc) " View Limit " WITH no-labels side-labels
   FRAME vlimit.


FORM
    "Limit value.:" ldeValue FORMAT ">>9" NO-LABEL 
                    lcValueDesc FORMAT "x(10)" NO-LABEL SKIP

WITH  OVERLAY ROW 8 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) lcLisTitle  
    SIDE-LABELS 
    FRAME lis.

orders = "By Customer    ,By Subscription".


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.
IF AVAILABLE Limit THEN ASSIGN
   Memory       = recid(Limit)
   must-print   = TRUE 
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No existing values" VIEW-AS ALERT-BOX.
   Ok = FALSE.

   IF llTerminated THEN RETURN.
   
   MESSAGE "Create limit" SKIP
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   SET Ok.
   
   IF NOT Ok THEN RETURN.
   IF llAdmin = FALSE THEN DO:
      MESSAGE ({&MSG_NOT_RIGHTS}) VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   RUN local-UPDATE-record.
   
   RUN local-find-first.
   IF AVAILABLE Limit THEN ASSIGN
      Memory       = recid(Limit)
      must-print   = TRUE 
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Limit WHERE recid(Limit) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Limit THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Limit).
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
        
        RUN local-find-this(FALSE).
        
        ASSIGN
        ufk = 0
        ufk[3] = 0 
        ufk[4] = 1068   WHEN llAdmin AND NOT llTerminated
        ufk[5] = 927    WHEN AVAIL Limit 
        ufk[6] = 1752   WHEN AVAIL Limit 
        ufk[8]= 8 
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Limit.Custnum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Limit.Custnum WITH FRAME sel.
      END.
      IF order = 2 THEN DO:
        CHOOSE ROW Limit.MsSeq ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Limit.MsSeq WITH FRAME sel.
      END.
      
      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND Limit WHERE recid(Limit) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Limit THEN
              ASSIGN FIRSTrow = i Memory = recid(Limit).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.
      
      IF LOOKUP(nap,"f4") > 0 AND ufk[4] > 0 THEN DO:
         
         RUN Syst/selectbox(
            "LIMIT FUNCTIONS",
            "Set limit",
            OUTPUT lcSelected).

         IF lcSelected EQ "Set limit" THEN DO:
            lcLisTitle = "Update Limit".
            RUN local-update-record.
            RUN local-disp-row.
         END.
         
         RUN local-find-this(FALSE).
         
         IF NOT AVAIL Limit THEN DO:
            RUN local-find-first.
            IF AVAILABLE Limit THEN ASSIGN
               Memory       = recid(Limit).
         END.
         must-print = TRUE.
         
         HIDE FRAME lis NO-PAUSE.
         ufkey = true.
         NEXT LOOP.
      END. 

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE Limit THEN DO:
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
                rtab[1] = recid(Limit)
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
           IF NOT AVAILABLE Limit THEN DO:
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
              rtab[FRAME-DOWN] = recid(Limit).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Limit WHERE recid(Limit) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Limit THEN DO:
           Memory = recid(Limit).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Limit THEN Memory = recid(Limit).
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
           FIND Limit WHERE recid(Limit) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
         
      
      ELSE IF LOOKUP(nap,"f5") > 0 AND ufk[5] > 0 THEN DO: 
         
         RUN local-find-this(FALSE).
         
         RUN Mc/memo
            (Limit.Custnum,
            "Limit",
            STRING(Limit.Custnum) + ";" + STRING(Limit.TMRuleSeq),
            "Limit").
         
         ufkey = true.
         NEXT LOOP.

      END.   
      
      ELSE IF LOOKUP(nap,"f6") > 0 AND ufk[5] > 0 THEN DO: 
      
         RUN local-find-this(FALSE).
         
         RUN Mc/eventsel("limit",
         "#BEGIN" + chr(255) + STRING(piCustnum) + chr(255) + 
         STRING(Limit.LimitType) + chr(255) + string(pitmruleseq) + 
         chr(255) + STRING(Limit.LimitID) + chr(255) + 
            STRING(YEAR(Limit.ToDate))       + '/'
              + STRING(MONTH(Limit.ToDate),'99') + '/'
                    + STRING(DAY(Limit.ToDate),'99')).

         ufkey = true.
         NEXT LOOP.

      END.   

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Limit) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Limit) must-print = TRUE.
        NEXT LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND Limit WHERE recid(Limit) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK NO-ERROR.
    ELSE
       FIND Limit WHERE recid(Limit) = rtab[frame-line(sel)] 
       NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN DO:
         FIND FIRST Limit WHERE
            Limit.MsSeq = piMsSeq AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX MsSeq NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 2 THEN DO:
         FIND FIRST Limit WHERE
            Limit.Custnum = piCustNum AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:
   
   IF order = 1 THEN DO:
         FIND LAST Limit WHERE
            Limit.MsSeq = piMsSeq AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX MsSeq NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 2 THEN DO:
         FIND LAST Limit WHERE
            Limit.Custnum = piCustNum AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   
   IF order = 1 THEN DO:
         FIND NEXT Limit WHERE
            Limit.MsSeq = piMsSeq AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX MsSeq NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 2 THEN DO:
         FIND NEXT Limit WHERE
            Limit.Custnum = piCustNum AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq 
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF order = 1 THEN DO:
         FIND PREV Limit WHERE
            Limit.MsSeq = piMsSeq AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX MsSeq NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 2 THEN DO:
         FIND PREV Limit WHERE
            Limit.Custnum = piCustNum AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq 
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-disp-row:
   
   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      Limit.Custnum
      Limit.MsSeq
      lcLimit
      lcValueType
      lcValue
      Limit.FromDate
      Limit.Todate
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
 
   lcLimit = lcLimitType.
   IF Limit.LimitID > 1 THEN lcLimit = LcLimit + " " + STRING(Limit.LimitID).
   
   RUN fFormatUnit(
      0, 
      Limit.LimitAmt,
      OUTPUT lcValueType,
      OUTPUT lcValue). 

   lcValueType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "Limit",
                               lcLimitType,
                               STRING(Limit.LimitAmt)).

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   
   RUN local-find-this (FALSE).

   CLEAR FRAME lis.

   FIND FIRST Limit WHERE
      Limit.MsSeq = piMsSeq AND
      Limit.LimitType = piLimitType AND
      Limit.TMRuleSeq = 0 AND
      Limit.Todate >= TODAY AND
      Limit.Custnum = piCustnum NO-LOCK NO-ERROR.
   IF AVAIL Limit THEN ldeValue = Limit.LimitAmt.
   ELSE ldeValue = 0.

   lcValueDesc = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
               "Limit",
               "Billing Permission",
               STRING(ldeValue)).

   UPDATE-LOOP:
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      ehto = 9. RUN Syst/ufkey.
  
      DISP ldeValue lcValueDesc WITH FRAME lis.
   
   UPDATE
      ldeValue 
   WITH FRAME lis EDITING:

      READKEY.
      
      nap = keylabel(lastkey).
      DEFINE VARIABLE lcNewLimits AS CHARACTER NO-UNDO.
      
      IF LOOKUP(nap,"f2") > 0 THEN .
      
      IF FRAME-FIELD = "ldeValue" AND KEYLABEL(LASTKEY) = "F9" THEN DO:
            
         RUN Syst/tmscodesel(INPUT "Limit",  
                              lcLimitType,
                              "Limit",
                              "",
                              TRUE,
                              "", 
                       OUTPUT lcCode,
                       OUTPUT lcCodeName).
         
         IF lcCode ne "" AND lcCode NE ? THEN DO:
               ldeValue= INT(lcCode). 
               lcValueDesc = lcCodeName.
            DISPLAY ldeValue lcValueDesc WITH FRAME lis.
         END.   

         ehto = 9.
         RUN Syst/ufkey.
         NEXT. 
         
      END.
      
      if lookup(nap,poisnap) > 0 THEN DO:
         
         IF ldeValue ENTERED THEN DO:
            
            lcValueDesc = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                        "Limit",
                        lcLimitType,
                        STRING(INPUT ldeValue)).
            
            IF lcValueDesc = "" THEN DO:   
               MESSAGE "Value not allowed due to business rules"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.

            DISPLAY lcValueDesc WITH FRAME lis.

         END.
   
         IF LOOKUP(nap,"f4,f8,x") > 0 THEN UNDO, LEAVE.

      END.

      APPLY LASTKEY.
   
   END.
   
   Ok = FALSE.
   
   MESSAGE "Update limit?" SKIP
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   SET Ok.
   
   IF NOT Ok THEN RETURN.

   IF ldeValue = 0 THEN DO:
      IF AVAIL Limit THEN 
      fSetLimit (
         ROWID(Limit),
         Limit.LimitAmt,
         FALSE, /* default value */ 
         Limit.FromDate,
         TODAY - 1).
   END.
   ELSE
   fCreateLimitHistory(
      piCustnum,
      piMsSeq,
      piLimitType,
      ldeValue,
      0, /* limit id */
      0, /* tmruleseq */
      FALSE, /* default value */
      TODAY,
      12/31/2049).
   
   LEAVE.
   
   END.

END PROCEDURE.
