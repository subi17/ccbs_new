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

DEFINE INPUT PARAM piTMRuleSeq AS INT NO-UNDO. 
DEFINE INPUT PARAM piMsSeq     AS INT NO-UNDO. 
DEFINE INPUT PARAM piCustNum   AS INT NO-UNDO. 
DEFINE INPUT PARAM piLimitType AS INT NO-UNDO. 

{Mc/lib/tokenlib.i}
{Func/dataformat.i}
{Func/flimitreq.i}

DEF NEW shared VAR siirto AS CHAR.
def buffer blimit for limit.

DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 2.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 14.
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
DEFINE VARIABLE lcValue      AS CHARACTER NO-UNDO format "x(12)". 
DEFINE VARIABLE lcHeader     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeValue     AS DECIMAL NO-UNDO EXTENT 2.  
DEFINE VARIABLE lcSelected   AS CHAR NO-UNDO.  
DEFINE VARIABLE lcRuleName   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTMRuleName AS CHARACTER NO-UNDO FORMAT "x(20)". 
DEFINE VARIABLE lcLisTitle   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLimitType  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ocResult     AS CHARACTER NO-UNDO. 

IF getTMSRight("CCSUPER,SYST") EQ "RW" THEN llAdmin = TRUE.

lcHeader = "TMR Limit". 
FIND FIRST TMRule WHERE 
   TMRule.Brand = gcBrand AND
   TMRule.TMRuleSeq = piTMRuleSeq NO-LOCK NO-ERROR.
IF AVAIL TMRule THEN DO:
   lcHeader = lcHeader + " - " + TMRule.Name.
   lcTMRuleName = TMRule.Name.
   IF TMRule.LimitSource NE 1 AND TMRule.LimitSource NE 4 THEN DO:
      MESSAGE "Not a customer based limit" VIEW-AS ALERT-BOX.
      RETURN.
   END.
END.

FORM
    lcLimit         COLUMN-LABEL "TMRLimit"
    lcValue         COLUMN-LABEL "Value"
    lcValueType     COLUMN-LABEL "Unit"
    Limit.FromDate  FORMAT       "99-99-9999" COLUMN-LABEL "From"
    Limit.ToDate    FORMAT       "99-99-9999" COLUMN-LABEL "To"
    Limit.DefValue  COLUMN-LABEL "Def" 
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN SCROLL 1
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + lcHeader + " "
    FRAME sel.

form 
   "TMRule.. ....:" lcTMRuleName SKIP
   "TMRLimit ....:" lcLimit SKIP
   "Limit Value .:" lcValue SKIP
   "Limit Perc ..:" Limit.LimitPerc SKIP
   "Limit Type ..:" Limit.LimitType lcLimitType SKIP
   "Unit ........:" TMRule.CounterAmount FORMAT "X(2)" lcValueType SKIP
   "Valid From ..:" Limit.FromDate FORMAT "99-99-9999" SKIP
   "Valid To ....:" Limit.ToDate FORMAT "99-99-9999" SKIP
   "Default Value:" Limit.DefValue  SKIP

WITH OVERLAY ROW 6 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc) " View Limit " WITH no-labels side-labels
   FRAME vlimit.


FORM
    "Limit 1 value.:" ldeValue[1] NO-LABEL SKIP
    ldeValue[2] AT 4 SKIP

WITH  OVERLAY ROW 8 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) lcLisTitle  
    SIDE-LABELS 
    FRAME lis.

orders = "By Customer    ,By Subscription".

RUN local-find-first.
IF AVAILABLE Limit THEN ASSIGN
   Memory       = recid(Limit)
   must-print   = TRUE 
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No existing values" VIEW-AS ALERT-BOX.
END.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

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
        ufk[3] = 2244 
        ufk[4] = 1068   WHEN llAdmin 
        ufk[5] = 927    WHEN AVAIL Limit 
        ufk[6] = 1752   WHEN AVAIL Limit 
        /*ufk[7]= 9016*/  ufk[8]= 8 
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW lcLimit {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) lcLimit WITH FRAME sel.
      END.
      
      IF rtab[FRAME-LINE] = ? THEN DO:
         nap = keylabel(LASTKEY).
        
        IF LOOKUP(nap,"f3") > 0 AND ufk[3] > 0 THEN DO: 
           RUN Mm/msrequest.p(40,?,0,piCustnum,0,"").
           must-print = true.
           ufkey = true.
           NEXT LOOP.
        END.   
         
        IF LOOKUP(nap,"f4") > 0 AND ufk[4] > 0 THEN DO: 
            
            RUN local-find-first.
            
            IF NOT AVAIL Limit THEN DO:
            
               RUN Syst/selectbox(
                  "TMR limit ADMIN FUNCTIONS",
                  "New limit",
                  OUTPUT lcSelected).

               IF lcSelected EQ "New limit" THEN DO:
                  lcLisTitle = "New Limit - " + TMRule.Name.
                  RUN local-update-record(true).
                  HIDE FRAME lis NO-PAUSE.
                  ufkey = true.
                  NEXT LOOP.
               END.
            END.
            ELSE DO:
               RUN Syst/selectbox(
                  "TMR limit ADMIN FUNCTIONS",
                  "Update limit",
                  OUTPUT lcSelected).

               IF lcSelected EQ "Update limit" THEN DO:
                  lcLisTitle = "Update Limit " + TMRule.Name.
                  RUN local-update-record(false).
                  HIDE FRAME lis NO-PAUSE.
                  ufkey = true.
                  NEXT LOOP.
               END.
            END.
        END.    
        
        IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.
        
        ELSE NEXT.
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
         
     IF LOOKUP(nap,"f3") > 0 AND ufk[3] > 0 THEN DO: 
        RUN Mm/msrequest(40,?,0,piCustnum,0,"").
        must-print = true.
        ufkey = true.
        NEXT LOOP.
     END.   
    
      ELSE IF LOOKUP(nap,"f4") > 0 AND ufk[4] > 0 THEN DO:
         
         IF AVAIL Limit THEN DO:
            RUN Syst/selectbox(
               "TMR limit ADMIN FUNCTIONS",
               "Update limit",
               OUTPUT lcSelected).

            IF lcSelected EQ "Update limit" THEN DO:
               lcLisTitle = "Update Limit " + TMRule.Name.
               RUN local-update-record(false).
            END.
         END. 
         
         HIDE FRAME lis NO-PAUSE.
         ufkey = true.
         NEXT LOOP.
      END. 
      
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
         STRING(Limit.LimitType) + chr(255) + string(tmrule.tmruleseq) + 
         chr(255) + STRING(Limit.LimitID)).

         ufkey = true.
         NEXT LOOP.

      END.   

      /* view */
      ELSE IF LOOKUP(nap,"enter,return") > 0 THEN DO:

        RUN local-view-record.
        ufkey = TRUE. 
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

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

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

   IF piCustNum NE 0 THEN DO:
      IF order = 1 THEN 
         FIND FIRST Limit WHERE
            Limit.Custnum = piCustNum AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.
   ELSE IF piMsSeq NE 0 THEN DO:
      IF order = 1 THEN 
         FIND FIRST Limit WHERE
            Limit.MsSeq = piMsSeq AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND FIRST Limit WHERE
            Limit.TMRuleSeq = piTMRuleSeq AND
            Limit.LimitType = piLimitType
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF piCustNum NE 0 THEN DO:
      IF order = 1 THEN 
         FIND LAST Limit WHERE
            Limit.Custnum = piCustNum AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.
   ELSE IF piMsSeq NE 0 THEN DO:
      IF order = 1 THEN 
         FIND LAST Limit WHERE
            Limit.MsSeq = piMsSeq AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND LAST Limit WHERE
            Limit.TMRuleSeq = piTMRuleSeq AND
            Limit.LimitType = piLimitType
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF piCustNum NE 0 THEN DO:
      IF order = 1 THEN 
         FIND NEXT Limit WHERE
            Limit.Custnum = piCustNum AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.
   ELSE IF piMsSeq NE 0 THEN DO:
      IF order = 1 THEN 
         FIND NEXT Limit WHERE
            Limit.MsSeq = piMsSeq AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND NEXT Limit WHERE
            Limit.TMRuleSeq = piTMRuleSeq AND
            Limit.LimitType = piLimitType
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF piCustNum NE 0 THEN DO:
      IF order = 1 THEN 
         FIND PREV Limit WHERE
            Limit.Custnum = piCustNum AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq 
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.
   ELSE IF piMsSeq NE 0 THEN DO:
      IF order = 1 THEN 
         FIND PREV Limit WHERE
            Limit.MsSeq = piMsSeq AND
            Limit.LimitType = piLimitType AND
            Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND PREV Limit WHERE
            Limit.TMRuleSeq = piTMRuleSeq AND
            Limit.LimitType = piLimitType
      USE-INDEX CustNum NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-disp-row:
   
   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      lcLimit
      lcValueType
      lcValue
      Limit.FromDate
      Limit.Todate
      Limit.DefValue
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
 
   lcLimit = "Limit " + STRING(Limit.LimitID).
   
   RUN fFormatUnit(
      TMRule.CounterAmount,
      Limit.LimitAmt,
      OUTPUT lcValueType,
      OUTPUT lcValue). 

      lcLimitType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "Limit",
                                  "LimitType",
                                  STRING(Limit.LimitType)).
END PROCEDURE.


PROCEDURE local-view-record:

   ufk = 0.
   RUN Syst/ufkey.

   RUN local-find-this(FALSE).
  
   CLEAR FRAME vlimit NO-PAUSE.
   
   RUN local-find-others.

   PAUSE 0.
  
   lcValue = TRIM(lcValue).

   DISP 
      lcTMRuleName
      lcLimit 
      lcValue 
      TMRule.CounterAmount lcValueType 
      Limit.FromDate 
      Limit.ToDate
      Limit.DefValue
      Limit.LimitPerc
      Limit.LimitType lcLimitType
   WITH FRAME vlimit.

   PAUSE MESSAGE "Press ENTER to continue".
  
   HIDE FRAME vlimit NO-PAUSE. 

END PROCEDURE.

PROCEDURE local-UPDATE-record:
  
   DEF INPUT PARAM ilNew AS LOG NO-UNDO. 
 
   CLEAR FRAME lis.
   DO i = 1 TO 2:
      ldeValue[i] = 0.
   END.

   IF ilNew EQ TRUE THEN DO:
     
      FIND FIRST TMRLimit WHERE 
         TMRLimit.TMRuleSeq = TMRule.TMRuleSeq AND
         TMRLimit.ToDate   >= TODAY NO-LOCK NO-ERROR.
      
      IF NOT AVAIL TMRLimit THEN DO:
         MESSAGE "This rule has no active limits!" VIEW-AS ALERT-BOX .
         RETURN.
      END.

      FOR EACH TMRLimit WHERE
         TMRLimit.TMRuleSeq = TMRule.TMRuleSeq AND
         TMRLimit.ToDate   >= TODAY NO-LOCK USE-INDEX LimitId
         i = 1 to 2:
         ldeValue[i] = TMRLimit.LimitAmt.
      END.
   
   END.
   ELSE DO: 
   
      FOR EACH bLimit WHERE
         bLimit.Custnum = piCustnum AND
         bLimit.LimitType = 1 AND
         bLimit.TMRuleSeq = TMRule.TMRuleSeq AND
         bLimit.ToDate >= TODAY NO-LOCK
         i = 1 to 2:
         ldeValue[i] = bLimit.LimitAmt.
      END.
   
   END.
   
   UPDATE-LOOP:
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      IF i >= 2 THEN 
         ldeValue[2]:label   in frame lis = "Limit 2 value.".
      ELSE ldeValue[2]:label in frame lis = "               ".
      
      ehto = 9. RUN Syst/ufkey.
  
   DO j = 1 TO i:
      DISP ldeValue[j] WITH FRAME lis.
   END.

   IF NOT ilNew OR TMRule.LimitSource EQ 4 THEN UPDATE
      ldeValue[1] WHEN i >= 1 
      ldeValue[2] WHEN i >= 2
   WITH FRAME lis EDITING:

      READKEY.
      
      nap = keylabel(lastkey).
      DEFINE VARIABLE lcNewLimits AS CHARACTER NO-UNDO.
      
      if lookup(nap,poisnap) > 0 THEN DO:
         
         
         IF ldeValue[FRAME-INDEX] ENTERED THEN DO:
   
            FIND FIRST TMRLimit WHERE 
               TMRLimit.TMRuleSeq = TMRule.TMRuleSeq AND
               TMRLimit.LimitId = FRAME-INDEX AND
               TMRLimit.ToDate >= TODAY
            NO-LOCK NO-ERROR.
            
            IF INPUT ldeValue[FRAME-INDEX] > TMRLimit.MaxValue THEN DO:
               MESSAGE "Value not allowed due to business rules" 
               VIEW-AS ALERT-BOX.
               NEXT.
            END.
            
            IF INPUT ldeValue[FRAME-INDEX] < TMRLimit.MinValue THEN DO:
               MESSAGE "Value not allowed due to business rules"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.

         END.
   
         IF LOOKUP(nap,"f4,f8,x") > 0 THEN UNDO, LEAVE.

      END.

      APPLY LASTKEY.
   
   END.
   
   ELSE DO:   
      
      ehto = 10.
      RUN Syst/ufkey.
      REPEAT:
         READKEY.
         IF LOOKUP(keylabel(lastkey),"1,f1") > 0 THEN LEAVE.
         IF LOOKUP(keylabel(lastkey),"4,f4") > 0 THEN RETURN.
      END.
   
   END.   
   
   
   j = i.
   IF j > 1 THEN DO WHILE j > 1:

      IF ldeValue[j] < ldeValue[j - 1] THEN DO:
         MESSAGE "Value not allowed due to business rules"
         VIEW-AS ALERT-BOX. 
         NEXT UPDATE-LOOP.
      END.
      j = j - 1.
   END.         

   Ok = FALSE.
   
   MESSAGE "Create a limit request?" SKIP
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   SET Ok.
   
   IF NOT Ok THEN DO:
     IF ilNew THEN RETURN. ELSE NEXT.
   END.
   /* No Termination request */

   DEFINE VARIABLE lcMode AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liMSReq AS INTEGER NO-UNDO.
   lcMode = (IF ilNew THEN "Create" ELSE "Update").

   liMsReq = fLimitRequest(
      ?,         /* msseq */
      piCustnum, /* custum */
      fMakeTS(), /* act.stamp */ 
      lcMode,    /* create, update */
      ldeValue,  /* new limit values */
      ilNew AND TMRule.LimitSource NE 4, /* default value */ 
      TMRule.TMRuleSeq, /* tmruleseq */
      1,         /* limit type */
      "4",       /* source of request */
      OUTPUT ocResult).
  

   IF liMsReq = 0 THEN
      MESSAGE ocResult VIEW-AS ALERT-BOX. 
   ELSE 
      MESSAGE
         "Limit Request ID is:" liMsReq
      VIEW-AS ALERT-BOX TITLE " REQUEST ADDED ".
  
   LEAVE.
   
   END.


END PROCEDURE.
