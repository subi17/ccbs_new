/* ----------------------------------------------------------------------
  MODULE .......: RerateLog
  TASK .........: browse table RerateLog
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 15.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RerateLog

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'RerateLog'}
{Func/timestamp.i}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRerateLog AS HANDLE NO-UNDO.
   lhRerateLog = BUFFER RerateLog:HANDLE.
   RUN StarEventInitialize(lhRerateLog).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhRerateLog).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR liCustNum    LIKE RerateLog.InvCust NO-UNDO.
DEF VAR lcCLI        LIKE RerateLog.CLI     NO-UNDO.
DEF VAR liMsSeq      LIKE RerateLog.MsSeq   NO-UNDO. 
DEF VAR ldaRateDate  AS DATE NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
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

DEF VAR lcStartTime  AS CHAR NO-UNDO.
DEF VAR lcEndTime    AS CHAR NO-UNDO.
DEF VAR lcCustName   AS CHAR NO-UNDO.
DEF VAR lcStarted    AS CHAR NO-UNDO. 

form
    RerateLog.InvCust
    RerateLog.CLI  
    RerateLog.EventSource 
    RerateLog.StartDate                     
    lcStarted    FORMAT "X(8)" COLUMN-LABEL "Time" 
    RerateLog.ChangedQty   
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " RERATE LOG "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    RerateLog.Brand       COLON 20
    RerateLog.InvCust     COLON 20 
       lcCustName NO-LABEL FORMAT "X(40)" SKIP
    RerateLog.MsSeq       COLON 20 FORMAT ">>>>>>>>"
    RerateLog.CLI         COLON 20 
    RerateLog.EventSource COLON 20
    RerateLog.PeriodBegin COLON 20
    RerateLog.PeriodEnd   COLON 20 SKIP(1)
    lcStartTime           COLON 20 
      LABEL "Started" FORMAT "X(20)" 
      "(" SPACE(0)  
      RerateLog.Started NO-LABEL
      SPACE(0) ")" SKIP
    lcEndTime           COLON 20 
      LABEL "Ended" FORMAT "X(20)" 
      "(" SPACE(0)  
      RerateLog.Ended NO-LABEL
      SPACE(0) ")" SKIP
    RerateLog.ChangedQty COLON 20 
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

   
{Func/brand.i}

FORM
   "Brand:" lcBrand skip
   "Customer:" liCustNum
      HELP "Enter invoice customer"
   WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Invoice Customer "
       COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM
   "Brand:" lcBrand skip
   "MSISDN:" lcCLI
      HELP "Enter MSISDN"
   WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
       COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FORM
   "Brand:" lcBrand skip
   "Rated:" ldaRateDate
      HELP "Enter rating date"
   WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Rating Date "
       COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.



cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE RerateLog THEN ASSIGN
   Memory       = recid(RerateLog)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rerate logs available" VIEW-AS ALERT-BOX.
      RETURN.   
   END.
   ELSE ASSIGN
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
        FIND RerateLog WHERE recid(RerateLog) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RerateLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(RerateLog).
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
        ufk[1] = 714  
        ufk[2] = 209
        ufk[3] = 28
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW RerateLog.InvCust ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) RerateLog.InvCust WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW RerateLog.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) RerateLog.CLI WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW RerateLog.StartDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) RerateLog.StartDate WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
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
        FIND RerateLog WHERE recid(RerateLog) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE RerateLog THEN
              ASSIGN FIRSTrow = i Memory = recid(RerateLog).
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
           IF NOT AVAILABLE RerateLog THEN DO:
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
                rtab[1] = recid(RerateLog)
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
           IF NOT AVAILABLE RerateLog THEN DO:
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
              rtab[FRAME-DOWN] = recid(RerateLog).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND RerateLog WHERE recid(RerateLog) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RerateLog THEN DO:
           Memory = recid(RerateLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE RerateLog THEN Memory = recid(RerateLog).
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
           FIND RerateLog WHERE recid(RerateLog) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.

       UPDATE lcBrand WHEN gcAllBrand
              liCustNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF liCustNum > 0 THEN DO:

          FIND FIRST RerateLog USE-INDEX InvCust WHERE 
                     RerateLog.InvCust >= liCustNum
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       
       UPDATE lcBrand WHEN gcAllBrand
              lcCLI WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF lcCLI > "" THEN DO:

          FIND FIRST RerateLog USE-INDEX CLI WHERE 
                     RerateLog.Brand = lcBrand    AND
                     RerateLog.CLI >= lcCLI     
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.
                           
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY column 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f3.
       DISPLAY lcBrand WITH FRAME F3.
       
       UPDATE lcBrand WHEN gcAllBrand
              ldaRateDate WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF ldaRateDate NE ? THEN DO:

          FIND FIRST RerateLog USE-INDEX StartDate WHERE 
                     RerateLog.Brand = lcBrand    AND
                     RerateLog.StartDate >= ldaRateDate
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(3) THEN NEXT BROWSE.
                           
          NEXT LOOP.
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRerateLog).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 5. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY RerateLog.InvCust.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRerateLog).

       RUN local-disp-row.
       xrecid = recid(RerateLog).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(RerateLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(RerateLog) must-print = TRUE.
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
      FIND RerateLog WHERE recid(RerateLog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND RerateLog WHERE recid(RerateLog) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN DO:
      FIND FIRST RerateLog USE-INDEX InvCust WHERE
                 RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.      
   ELSE IF order = 2 THEN DO:
      FIND FIRST RerateLog USE-INDEX CLI WHERE
                 RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 3 THEN DO:
      FIND FIRST RerateLog USE-INDEX StartDate WHERE
                 RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN DO:
      FIND LAST RerateLog USE-INDEX InvCust WHERE
                RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.      
   ELSE IF order = 2 THEN DO:
      FIND LAST RerateLog USE-INDEX CLI WHERE
                RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 3 THEN DO:
      FIND LAST RerateLog USE-INDEX StartDate WHERE
                RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN DO:
      FIND NEXT RerateLog USE-INDEX InvCust WHERE
                RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.      
   ELSE IF order = 2 THEN DO:
      FIND NEXT RerateLog USE-INDEX CLI WHERE
                RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 3 THEN DO:
      FIND NEXT RerateLog USE-INDEX StartDate WHERE
                RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN DO:
      FIND PREV RerateLog USE-INDEX InvCust WHERE
                RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.      
   ELSE IF order = 2 THEN DO:
      FIND PREV RerateLog USE-INDEX CLI WHERE
                RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 3 THEN DO:
      FIND PREV RerateLog USE-INDEX StartDate WHERE
                RerateLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       RerateLog.InvCust 
       RerateLog.CLI
       RerateLog.EventSource
       RerateLog.StartDate
       lcStarted
       RerateLog.ChangedQty
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   DEF VAR ldaDate AS DATE NO-UNDO.
   DEF VAR liTime  AS INT  NO-UNDO. 
   
   fSplitTS(RerateLog.Started,
            OUTPUT ldaDate,
            OUTPUT liTime).
   lcStarted = STRING(liTime,"hh:mm:ss").         
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      ASSIGN
         lcStartTime = fTS2HMS(RerateLog.Started)
         lcEndTime   = fTS2HMS(RerateLog.Ended).
         
      FIND FIRST Customer WHERE Customer.CustNum = RerateLog.InvCust 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
          lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                        BUFFER Customer).
      ELSE lcCustName = "".
      
      DISP 
           RerateLog.Brand
           RerateLog.InvCust
           lcCustName
           RerateLog.MsSeq
           RerateLog.CLI
           RerateLog.EventSource 
           RerateLog.PeriodBegin 
           RerateLog.PeriodEnd   
           lcStartTime
           RerateLog.Started
           lcEndTime
           RerateLog.Ended
           RerateLog.ChangedQty
      WITH FRAME lis.

      ASSIGN 
         ehto = 0
         ufk  = 0
         ufk[8] = 8.
      RUN ufkey.
      
      IF toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.

