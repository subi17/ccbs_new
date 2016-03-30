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
{Func/dataformat.i}
{Func/cparam2.i}

DEFINE INPUT PARAM piTMRuleSeq AS INT NO-UNDO. 
DEFINE INPUT PARAM piMsSeq     AS INT NO-UNDO. 
DEFINE INPUT PARAM piCustNum   AS INT NO-UNDO. 

DEF NEW shared VAR siirto AS CHAR.

DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 15.
DEFINE VARIABLE order        AS INTEGER                 NO-UNDO  init 3.
DEFINE VARIABLE orders       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE maxOrder     AS INTEGER                 NO-UNDO  init 3.
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
DEFINE VARIABLE ok           AS LOGICAL format "Yes/No" NO-UNDO.
DEFINE VARIABLE lcCustnum    AS INT NO-UNDO. 
DEFINE VARIABLE liMsSeq      AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcValueType  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcValue      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcHead     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeAmount    LIKE TMCounter.Amount. 
DEFINE VARIABLE lcAmount     AS CHARACTER NO-UNDO INIT "".
DEFINE VARIABLE ldeAmountPrev AS DECIMAL NO-UNDO.

FIND FIRST TMRule WHERE
   TMRule.TMRuleSeq = piTMRuleSeq NO-LOCK NO-ERROR.
IF AVAIL TMRule THEN lcHead = "TMC - " + TMRule.Name.
ELSE lcHead = "TMC - Unknown counter".

DEFINE VARIABLE lcHeader AS CHARACTER NO-UNDO. 
lcHeader = lcHead.

DEFINE TEMP-TABLE ttTMcounter LIKE TMCounter
INDEX Amount Amount desc
INDEX CustNum CustNum ASC TMRuleSeq ASC ToDate DESC Amount DESC
INDEX MsSeq MsSeq ASC TMRuleSeq ASC ToDate DESC Amount DESC.

disp " Please wait... " with frame x overlay row 7 centered.
pause 0.
IF piMsSeq NE 0 AND
   STRING(piTMRuleSeq) <> fCParamC("TMQueueDSSUpsell") THEN DO:
   FOR EACH TMCounter WHERE
      TMCounter.MsSeq = piMsSeq NO-LOCK:
      CREATE ttTMCounter.
      BUFFER-COPY TMCounter TO ttTMCounter.
   END.
END.
ELSE IF piCustNum NE 0 THEN DO:
   FOR EACH TMCounter WHERE
      TMCounter.CustNum = piCustnum NO-LOCK:
      CREATE ttTMCounter.
      BUFFER-COPY TMCounter TO ttTMCounter.
   END.
END.
ELSE
FOR EACH TMCounter where NO-LOCK:
   CREATE ttTMCounter.
   BUFFER-COPY TMCounter TO ttTMCounter.
END.

HIDE FRAME x NO-PAUSE.

FORM
    ttTMCounter.Custnum   COLUMN-LABEL "CustNbr"
    ttTMCounter.MsSeq     COLUMN-LABEL "Subscr.ID" 
    ttTMCounter.FromDate  FORMAT "99/99/99" COLUMN-LABEL "From"
    ttTMCounter.ToDate    FORMAT "99/99/99" COLUMN-LABEL "To"
    lcValueType           FORMAT "X(12)" COLUMN-LABEL "Unit"
    lcValue               FORMAT "X(16)" COLUMN-LABEL "Usage"
    ttTMCounter.Limitamt  FORMAT ">>>>>9" COLUMN-LABEL "Limit"
    ttTMCounter.LimitId   FORMAT ">9"  COLUMN-LABEL "LID"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN SCROLL 1
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + lcHeader + " "
    FRAME sel.

FORM /* seek TMCounter  BY TMCounterId and TMCounterOffice */
    "CustNbr:" lcCustnum FORMAT ">>>>>>>9" 
    HELP "Enter Customer Number" SKIP
   WITH OVERLAY row 4 col 2 
   TITLE COLOR VALUE(ctc) " FIND Customer "
   COLOR VALUE(cfc)
   NO-LABELS FRAME f1.

FORM /* seek  MSBalance */
   "Subscr.ID:" liMsSeq FORMAT ">>>>>>>9"
      HELP "Enter Subscription ID"
   WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Subscription "
      COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FORM /* seek TMCounter  BY TMCounterId and TMCounterOffice */
    "Usage exceeds....:" ldeAmount FORMAT ">>>>>>9.999"
   HELP "Enter value" SKIP
   WITH OVERLAY row 4 col 2 
   TITLE COLOR VALUE(ctc) " COUNTER FILTER "
   COLOR VALUE(cfc)
   NO-LABELS FRAME f3.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = " By Customer    , By Subscription, By Usage       ".

RUN local-find-first.
IF AVAILABLE ttTMCounter THEN ASSIGN
   Memory       = recid(ttTMCounter)
   must-print   = TRUE 
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No existing values" VIEW-AS ALERT-BOX.
   RETURN.
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
        FIND ttTMCounter WHERE recid(ttTMCounter) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttTMCounter THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttTMCounter).
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
         ufk = 0
         ufk[1]= 714 WHEN (piCustnum EQ 0 AND piMsSeq EQ 0)
         ufk[2]= 1645 WHEN (piMsSeq EQ 0)
         ufk[3]= 9016
         ufk[5]= 0 
         ufk[6]= 0 
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttTMCounter.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttTMCounter.CustNum WITH FRAME sel.
      END.
      
      IF order = 2 THEN DO:
         CHOOSE ROW ttTMCounter.MsSeq {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttTMCounter.MsSeq WITH FRAME sel.
      END.
      
      IF order = 3 THEN DO:
         CHOOSE ROW lcValue {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) lcValue WITH FRAME sel.
      END.
      
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
         ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
         FIND ttTMCounter WHERE recid(ttTMCounter) = Memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-PREV.
            IF AVAILABLE ttTMCounter THEN
               ASSIGN FIRSTrow = i Memory = recid(ttTMCounter).
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
            IF NOT AVAILABLE ttTMCounter THEN DO:
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
                  rtab[1] = recid(ttTMCounter)
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
            IF NOT AVAILABLE ttTMCounter THEN DO:
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
               rtab[FRAME-DOWN] = recid(ttTMCounter).
               /* save RECID of uppermost ROW */
               Memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND ttTMCounter WHERE recid(ttTMCounter) = Memory NO-LOCK NO-ERROR.
         RUN local-find-PREV.
         IF AVAILABLE ttTMCounter THEN DO:
            Memory = recid(ttTMCounter).

            /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-PREV.
               IF AVAILABLE ttTMCounter THEN Memory = recid(ttTMCounter).
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
            FIND ttTMCounter WHERE recid(ttTMCounter) = Memory NO-LOCK.
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
         ASSIGN
            lcCustnum  = 0.

         SET lcCustnum WITH FRAME f1.
         HIDE FRAME f1 NO-PAUSE.
         IF lcCustnum ENTERED THEN
         DO:
            IF lcCustnum NE 0 THEN 
            FIND FIRST ttTMCounter NO-LOCK WHERE 
                     ttTMCounter.Custnum = lcCustnum 
            NO-ERROR.
            
            IF NOT AVAILABLE ttTMCounter THEN DO:
               BELL.
               MESSAGE "NOT FOUND !".
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
          
            ASSIGN 
               memory     = recid(ttTMCounter) 
               must-print = TRUE.
            NEXT LOOP.
         END.
      END. 
     
      /* Search BY MsSeq */
      ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
      THEN DO ON ENDKEY UNDO, NEXT LOOP:
             
         cfc = "puyr". RUN Syst/ufcolor.
         ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
         CLEAR FRAME f2.
         ASSIGN
               liMsSeq = 0.

         SET liMsSeq WITH FRAME f2.
         HIDE FRAME f2 NO-PAUSE.
         IF liMsSeq ENTERED THEN
         DO:
           
            IF liMsSeq NE 0 THEN DO:
               FIND FIRST MobSub WHERE MobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
               IF NOT AVAILABLE MobSub THEN DO:
                  BELL.
                  MESSAGE "NOT FOUND !".
                  PAUSE 1 NO-MESSAGE.
                  NEXT BROWSE.
               END.
              
               FIND FIRST ttTMCounter NO-LOCK WHERE 
                         ttTMCounter.MsSeq = MobSub.MsSeq 
               NO-ERROR.
               IF NOT AVAILABLE ttTMCounter THEN DO:
                  BELL.
                  MESSAGE "NOT FOUND !".
                  PAUSE 1 NO-MESSAGE.
                  NEXT BROWSE.
               END.
            END.
           
            ASSIGN 
               memory     = recid(ttTMCounter) 
               must-print = TRUE.
            NEXT LOOP.
         END.
      END. 
           
      /* Filter by Usage */
      ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
      THEN DO ON ENDKEY UNDO, NEXT LOOP:
             
         cfc = "puyr". RUN Syst/ufcolor.
         ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
         CLEAR FRAME f3.
         ldeAmountPrev = ldeAmount. 
         DISP ldeAmount WITH FRAME f3.
         SET ldeAmount WITH FRAME f3.
         HIDE FRAME f3 NO-PAUSE.
         IF ldeAmount ENTERED THEN
         DO:
           
            IF ldeAmount >= 0 THEN DO:
                 
               RUN local-FIND-FIRST.
               
               IF NOT AVAILABLE ttTMCounter THEN DO:
                  BELL.
                  MESSAGE "NOT FOUND !".
                  PAUSE 1 NO-MESSAGE.
                  must-print = true. 
                  ldeAmount = ldeAmountPrev. 
                  NEXT BROWSE.
               END.
               
               RUN fFormatUnit(
                 TMRule.CounterAmount,
                 ldeAmount,
                 output lcValueType, 
                 output lcAmount).
                
               lcHeader = lcHead + " (Usage >= " + TRIM(lcAmount) + ")".
               HIDE FRAME sel NO-PAUSE.
               VIEW FRAME sel.
            END.
            
            ASSIGN 
                  memory     = recid(ttTMCounter) 
                  must-print = TRUE.
            NEXT LOOP.
         END.
      END.
           
      ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
         RUN local-find-FIRST.
         ASSIGN Memory = recid(ttTMCounter) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
         RUN local-find-LAST.
         ASSIGN Memory = recid(ttTMCounter) must-print = TRUE.
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
      FIND ttTMCounter WHERE recid(ttTMCounter) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttTMCounter WHERE recid(ttTMCounter) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF piCustNum NE 0 THEN DO:
      IF order = 1 THEN 
         FIND FIRST ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
         FIND FIRST ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
         FIND FIRST ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.
   ELSE IF piMsSeq NE 0 THEN DO:
      IF order = 1 THEN 
         FIND FIRST ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
         FIND FIRST ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
         FIND FIRST ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND FIRST ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
      FIND FIRST ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
      FIND FIRST ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF piCustNum NE 0 THEN DO:
      IF order = 1 THEN 
         FIND LAST ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
         FIND LAST ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
         FIND LAST ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.
   ELSE IF piMsSeq NE 0 THEN DO:
      IF order = 1 THEN 
         FIND LAST ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
         FIND LAST ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
         FIND LAST ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND LAST ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
      FIND LAST ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
      FIND LAST ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF piCustNum NE 0 THEN DO:
      IF order = 1 THEN 
         FIND NEXT ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
         FIND NEXT ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
         FIND NEXT ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.
   ELSE IF piMsSeq NE 0 THEN DO:
      IF order = 1 THEN 
         FIND NEXT ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
         FIND NEXT ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
         FIND NEXT ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND NEXT ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
      FIND NEXT ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
      FIND NEXT ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF piCustNum NE 0 THEN DO:
      IF order = 1 THEN 
         FIND PREV ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
         FIND PREV ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
         FIND PREV ttTMCounter WHERE
            ttTMCounter.Custnum = piCustNum AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.
   ELSE IF piMsSeq NE 0 THEN DO:
      IF order = 1 THEN 
         FIND PREV ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
         FIND PREV ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
         FIND PREV ttTMCounter WHERE
            ttTMCounter.MsSeq = piMsSeq AND
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN 
         FIND PREV ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
      USE-INDEX CustNum NO-LOCK NO-ERROR.
      IF order = 2 THEN 
      FIND PREV ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX MsSeq NO-LOCK NO-ERROR.
      IF order = 3 THEN 
      FIND PREV ttTMCounter WHERE
            ttTMCounter.TMRuleSeq = piTMRuleSeq
            AND ttTMCounter.Amount >= ldeAmount
         USE-INDEX Amount NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      ttTMCounter.CustNum
      ttTMCounter.MsSeq
      ttTMCounter.FromDate
      ttTMCounter.Todate
      lcValueType
      lcValue 
      ttTMCounter.LimitAmt
      ttTMCounter.LimitId WHEN ttTMCounter.LimitId NE 0
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   RUN fFormatUnit(
      TMRule.CounterAmount,
      ttTMCounter.Amount,
      OUTPUT lcValueType,
      OUTPUT lcValue). 

END PROCEDURE.
