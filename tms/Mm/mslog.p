/* ----------------------------------------------------------------------
  MODULE .......: mslog
  TASK .........: VIEW table mslog
  APPLICATION ..: 
  AUTHOR .......: 
  CREATED ......: 23.1.2006 jp
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'BillType'}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMSLog AS HANDLE NO-UNDO.
   lhMSLog = BUFFER MSLog:HANDLE.
   RUN StarEventInitialize(lhMSLog).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhMSLog).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF INPUT PARAMETER   iiMSSeq    LIKE MSLog.MSSeq           NO-UNDO.
DEF INPUT PARAMETER   icLogType  LIKE MSLog.LogType         NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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

form
    MSLog.MSSeq     /* COLUMN-LABEL FORMAT */
    MSLog.LogType     /* COLUMN-LABEL FORMAT */
    MsLog.LogStatus
    MsLog.EventValue
    MsLog.UserCode
    Mslog.EventStamp

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  BILLING ITEMS MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    MSLog.MSSeq     /* LABEL FORMAT */
    MSLog.LogType    /* LABEL FORMAT */
    MsLog.LogStatus
    MsLog.EventValue
    MsLog.UserCode
    Mslog.EventStamp
                

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


RUN LOCAL-FIND-FIRST.
IF AVAILABLE MSLog THEN ASSIGN
   Memory       = recid(MSLog)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF NOT AVAIL MSLog THEN DO:
      MESSAGE 
      "MSLog not Available!"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No billing items available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a MSSeq  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR MSLog.MSSeq
           VALIDATE
              (MSLog.MSSeq NOT ENTERED OR
              NOT CAN-FIND(MSLog using  MSLog.MSSeq),
              "Billing RepLogType " + string(INPUT MSLog.MSSeq) +
              " already exists !").
           IF INPUT FRAME lis MSLog.MSSeq = "" THEN 
           LEAVE add-row.
           CREATE MSLog.
           ASSIGN
           MSLog.MSSeq = INPUT FRAME lis MSLog.MSSeq.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMSLog).

           ASSIGN
           Memory = recid(MSLog)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MSLog
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MSLog THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MSLog WHERE recid(MSLog) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MSLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MSLog).
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
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.
      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MSLog.MSseq ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MSLog.MSseq WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MSLog.LogType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MSLog.LogType WITH FRAME sel.
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
        FIND MSLog WHERE recid(MSLog) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MSLog THEN
              ASSIGN FIRSTrow = i Memory = recid(MSLog).
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
           IF NOT AVAILABLE MSLog THEN DO:
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
                rtab[1] = recid(MSLog)
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
           IF NOT AVAILABLE MSLog THEN DO:
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
              rtab[FRAME-DOWN] = recid(MSLog).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MSLog WHERE recid(MSLog) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MSLog THEN DO:
           Memory = recid(MSLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MSLog THEN Memory = recid(MSLog).
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
           FIND MSLog WHERE recid(MSLog) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       MSLog.MSSeq MSLog.LogType     MsLog.LogStatus
           MsLog.EventValue
               MsLog.UserCode
                   Mslog.EventStamp
                   .

       RUN local-find-NEXT.
       IF AVAILABLE MSLog THEN Memory = recid(MSLog).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE MSLog THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(MSLog).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       MSLog.MSSeq MSLog.LogType     MsLog.LogStatus
           MsLog.EventValue
               MsLog.UserCode
                   Mslog.EventStamp
                   .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMSLog).

           DELETE MSLog.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST MSLog
           /* srule */) THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSLog).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY MSLog.MSSeq MSLog.Logtype     MsLog.LogStatus
           MsLog.EventValue
           MsLog.UserCode
           Mslog.EventStamp
                   .

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSLog).

       RUN local-disp-row.
       xrecid = recid(MSLog).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MSLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MSLog) must-print = TRUE.
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
      FIND MSLog WHERE recid(MSLog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MSLog WHERE recid(MSLog) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 AND iimsseq > 0 THEN 
       FIND FIRST MSLog WHERE MsLog.MSSEq = iiMSSeq NO-LOCK NO-ERROR.
       
       ELSE IF Order = 1 AND icLogType > "" THEN 
       FIND FIRST MSLog WHERE MSLog.LogType = icLogType NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 AND icLogType > "" THEN 
       FIND FIRST MSLog USE-INDEX LogType NO-LOCK NO-ERROR.
       
       ELSE IF ORder = 2 AND iiMsseq > 0 THEN 
       FIND FIRST MSLog WHERE MsLog.MSSEq = iiMSSeq NO-LOCK NO-ERROR.

       ELSE IF Order = 1 THEN 
       FIND FIRST MSLog NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN 
       FIND FIRST MSLog USE-INDEX LogType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 AND iimsseq > 0 THEN 
       FIND LAST  MSLog WHERE MsLog.MSSEq = iiMSSeq NO-LOCK NO-ERROR.
       
       ELSE IF Order = 1 AND icLogType > "" THEN 
       FIND LAST  MSLog WHERE MSLog.LogType = icLogType NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 AND icLogType > "" THEN 
       FIND LAST  MSLog USE-INDEX LogType NO-LOCK NO-ERROR.
       
       ELSE IF ORder = 2 AND iiMsseq > 0 THEN 
       FIND LAST  MSLog WHERE MsLog.MSSEq = iiMSSeq NO-LOCK NO-ERROR.

       ELSE IF Order = 1 THEN 
       FIND LAST MSLog NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN 
       FIND LAST MSLog USE-INDEX LogType NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 AND iimsseq > 0 THEN 
       FIND NEXT  MSLog WHERE MsLog.MSSEq = iiMSSeq NO-LOCK NO-ERROR.
       
       ELSE IF Order = 1 AND icLogType > "" THEN 
       FIND NEXT  MSLog WHERE MSLog.LogType = icLogType NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 AND icLogType > "" THEN 
       FIND NEXT  MSLog USE-INDEX LogType NO-LOCK NO-ERROR.
       
       ELSE IF ORder = 2 AND iiMsseq > 0 THEN 
       FIND NEXT  MSLog WHERE MsLog.MSSEq = iiMSSeq NO-LOCK NO-ERROR.

       ELSE IF Order = 1 THEN 
       FIND NEXT MSLog NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN 
       FIND NEXT MSLog USE-INDEX LogType NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 AND iimsseq > 0 THEN 
       FIND PREV MSLog WHERE MsLog.MSSEq = iiMSSeq NO-LOCK NO-ERROR.
       
       ELSE IF Order = 1 AND icLogType > "" THEN 
       FIND PREV  MSLog WHERE MSLog.LogType = icLogType NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 AND icLogType > "" THEN 
       FIND PREV  MSLog USE-INDEX LogType NO-LOCK NO-ERROR.
       
       ELSE IF ORder = 2 AND iiMsseq > 0 THEN 
       FIND PREV  MSLog WHERE MsLog.MSSEq = iiMSSeq NO-LOCK NO-ERROR.

       ELSE IF Order = 1 THEN 
       FIND PREV MSLog NO-LOCK NO-ERROR.
       
       ELSE IF order = 2 THEN 
       FIND PREV MSLog USE-INDEX LogType NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       MSLog.MSseq
       MSLog.LogType
       MsLog.LogStatus
       MsLog.EventValue
       MsLog.UserCode
       Mslog.EventStamp

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          MSLog.LogType
          MsLog.LogStatus
          MsLog.EventValue
          MsLog.UserCode
          Mslog.EventStamp
                          
      WITH FRAME lis.
      MESSAGE "PRESS ENTER TO CONTINUE " . pause NO-MESSAGE.
      
      LEAVE.
   END.
END PROCEDURE.

