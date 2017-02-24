/* ----------------------------------------------------------------------
  MODULE .......: alarm_tms
  TASK .........: Cause an alarm, if TMS does exception
  APPLICATION ..: tms
  AUTHOR .......: Janne Tourunen
  CREATED ......: 23.02.2017
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable Alarm
{commali.i}
{eventval.i}
{timestamp.i}

DEF SHARED VAR siirto AS CHAR.

DEF VAR liAlarmID     AS INT                    NO-UNDO.
DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 1.
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
DEF VAR ldtDate       AS DATE                   NO-UNDO.
DEF VAR liTime        AS INT                    NO-UNDO. 
DEF VAR lcCancelTime  AS CHAR                   NO-UNDO.
DEF VAR lcSetTime     AS CHAR                   NO-UNDO.
DEF VAR lcClearTime   AS CHAR                   NO-UNDO.
DEF VAR lcResetTime   AS CHAR                   NO-UNDO.

FORM
    Alarm.AlarmID    FORMAT ">>>>>>>9"
    Alarm.CurrentStatus  FORMAT ">9"
    Alarm.Severity  FORMAT ">9"
    Alarm.SettingTime
    Alarm.AlarmText    FORMAT "X(30)"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  TMS ALARM  " +
       string(pvm,"99-99-99") + " "
    FRAME sel.

form
    Alarm.AlarmID COLON 20        
    Alarm.AlarmText COLON 20      
    Alarm.CancelTime COLON 20  lcCancelTime FORMAT "X(20)" NO-LABEL
    Alarm.ClearTime COLON 20   lcClearTime  FORMAT "X(20)" NO-LABEL  
    Alarm.CurrentStatus COLON 20  
    Alarm.Parameters COLON 20     
    Alarm.ProgramBlock COLON 20   
    Alarm.ResetTime COLON 20   lcResetTime FORMAT "X(20)" NO-LABEL  
    Alarm.SettingTime COLON 20 lcSetTime   FORMAT "X(20)" NO-LABEL
    Alarm.Severity COLON 20
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  Alarm */
    "DumpID:" liAlarmID 
    HELP "Enter AlarmID "
    WITH row 4 col 1 TITLE COLOR VALUE(ctc) " FIND Alarm ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


IF gcHelpParam > "" THEN ASSIGN
   FrmRow  = 3
   FrmDown = 11.

cfc = "sel". RUN ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE Alarm THEN ASSIGN
   Memory       = recid(Alarm)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   
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

        FIND Alarm WHERE recid(Alarm) = Memory NO-LOCK NO-ERROR.

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Alarm THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Alarm).
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
        ufk[1] = 816
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW alarm.alarmid {uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) alarm.alarmid WITH FRAME sel.
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
        FIND Alarm WHERE recid(Alarm) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Alarm THEN
              ASSIGN FIRSTrow = i Memory = recid(Alarm).
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
           IF NOT AVAILABLE Alarm THEN DO:
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
                rtab[1] = recid(Alarm)
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
           IF NOT AVAILABLE Alarm THEN DO:
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
              rtab[FRAME-DOWN] = recid(Alarm).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Alarm WHERE recid(Alarm) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Alarm THEN DO:
           Memory = recid(Alarm).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Alarm THEN Memory = recid(Alarm).
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
           FIND Alarm WHERE recid(Alarm) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.p.
       ehto = 9. RUN ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       SET liAlarmID WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF liAlarmID > 0 THEN DO:
          FIND FIRST Alarm WHERE 
                     Alarm.AlarmID >= liAlarmID
          NO-LOCK NO-ERROR.

          /* IF NOT fRecFound(1) THEN NEXT BROWSE. */

          NEXT LOOP.
       END.
     END. /* Search-1 */
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.p.
       cfc = "lis". RUN ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY Alarm.AlarmID.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(Alarm).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Alarm) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Alarm) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */
HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

IF gcHelpParam > "" THEN DO:
   IF xRecid NE ? THEN DO:
      FIND FIRST Alarm WHERE RECID(Alarm) = xRecid NO-LOCK.
      siirto = STRING(Alarm.AlarmId).
   END.   
END.
  

ehto = 4.
RUN ufkey.p.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND Alarm WHERE recid(Alarm) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Alarm WHERE recid(Alarm) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST Alarm USE-INDEX AlarmID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST Alarm USE-INDEX AlarmID NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT Alarm USE-INDEX AlarmID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV Alarm USE-INDEX AlarmID NO-LOCK NO-ERROR. 
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       Alarm.AlarmId
       Alarm.CurrentStatus
       Alarm.Severity
       Alarm.SettingTime
       Alarm.AlarmText
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

    fSplitTS(Alarm.SettingTime,
             OUTPUT ldtDate,
             OUTPUT liTime).

END PROCEDURE.


PROCEDURE local-UPDATE-record:

   DEF VAR liInfoPos AS INT  NO-UNDO.
   DEF VAR liLine    AS INT  NO-UNDO.

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      ASSIGN 
         lcCancelTime = (IF Alarm.CancelTime > 0 THEN
                           fTS2HMS(Alarm.CancelTime)
                         ELSE "")
         lcSetTime = (IF Alarm.SettingTime > 0 THEN
                           fTS2HMS(Alarm.SettingTime)
                      ELSE "")
         lcClearTime = (IF Alarm.ClearTime > 0 THEN
                           fTS2HMS(Alarm.ClearTime)
                      ELSE "")
         lcResetTime = (IF Alarm.ResetTime > 0 THEN
                           fTS2HMS(Alarm.ResetTime)
                      ELSE "").
      DISP 
        Alarm.AlarmID        
        Alarm.AlarmText      
        Alarm.CancelTime  lcCancelTime  
        Alarm.ClearTime   lcClearTime   
        Alarm.CurrentStatus  
        Alarm.Parameters     
        Alarm.ProgramBlock   
        Alarm.ResetTime   lcResetTime   
        Alarm.SettingTime lcSetTime   
        Alarm.Severity       
      WITH FRAME lis.

      ASSIGN 
         ehto = 0
         ufk  = 0
         ufk[8] = 8.
      RUN ufkey.
      

      IF toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.
