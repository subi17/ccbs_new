/* ----------------------------------------------------------------------
  MODULE .......: DumpLog
  TASK .........: browse table DumpLog
  APPLICATION ..: TMS
  AUTHOR .......: jannetou
  CREATED ......: 01.02.2013
  CHANGED ......: 
  Version ......: M1
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable DumpLog

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DumpLog'}
{Func/timestamp.i}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDumpLog AS HANDLE NO-UNDO.
   lhDumpLog = BUFFER DumpLog:HANDLE.
   RUN StarEventInitialize(lhDumpLog).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhDumpLog).
   END.

END.
DEF INPUT PARAMETER iiDumpID AS INT NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.

DEF VAR lcTime       AS CHAR NO-UNDO.
DEF VAR ldtDate      AS DATE NO-UNDO.
DEF VAR liTime       AS INT  NO-UNDO. 
DEF VAR lcInfo       AS CHAR NO-UNDO.
DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcCode       AS CHAR NO-UNDO.
DEF VAR lcLargeInfo  AS CHAR NO-UNDO.
DEF VAR liDumpID     AS INT NO-UNDO.
DEF VAR lcCreateStart AS CHAR NO-UNDO FORMAT "X(25)".
DEF VAR lcCreateEnd AS CHAR NO-UNDO FORMAT "X(25)".

form
    DumpLog.DumpId FORMAT ">>9" COLUMN-LABEL "ID"
    DumpLog.DumpLogId  FORMAT ">>>>>9" COLUMN-LABEL "Key"                    
    DumpLog.CreateStart COLUMN-LABEL "Creation TS"
    DumpLog.DumpType COLUMN-LABEL "Mode"
    DumpLog.DumpLogStatus COLUMN-LABEL "St"
    DumpLog.FileName FORMAT "X(40)" COLUMN-LABEL "Filename"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "DUMP LOG "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    DumpLog.DumpId COLON 20
    DumpLog.DumpLogId COLON 20
    DumpLog.CreateStart COLON 20 lcCreateStart NO-LABEL
    DumpLog.CreateEnd  COLON 20 lcCreateEnd NO-LABEL
    DumpLog.DumpType COLON 20
    DumpLog.DumpLogStatus COLON 20
      lcStatus NO-LABEL FORMAT "X(30)"
    DumpLog.FileName COLON 20
    DumpLog.Filesize COLON 20
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM
    lcLargeInfo AT 2 NO-LABEL
       VIEW-AS EDITOR SIZE-CHARS 70 BY 17
WITH OVERLAY ROW 1 centered
    COLOR VALUE(cfc) TITLE " VIEW INFO " 
    SIDE-LABELS FRAME fInfo.

form /* seek  DumpLog */
    "DumpID:" liDumpID 
    HELP "Enter Dump ID "
    WITH row 4 col 1 TITLE COLOR VALUE(ctc) " FIND Dump ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FUNCTION fStatusName RETURNS LOGICAL
   (INPUT iiStatus AS INT):

   lcStatus = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "ActionLog",
                               "ActionStatus",
                               STRING(iiStatus)).
   DISP lcStatus WITH FRAME lis.
   PAUSE 0.
END.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE DumpLog THEN ASSIGN
   Memory       = recid(DumpLog)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No logs available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND DumpLog WHERE recid(DumpLog) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DumpLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(DumpLog).
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
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        IF iiDumpID > 0 THEN ASSIGN 
           ufk[1] = 0
           ufk[3] = 0.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      CHOOSE ROW DumpLog.DumpId {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
      COLOR DISPLAY VALUE(ccc) DumpLog.DumpId WITH FRAME sel.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE DumpLog THEN DO:
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
                rtab[1] = recid(DumpLog)
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
           IF NOT AVAILABLE DumpLog THEN DO:
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
              rtab[FRAME-DOWN] = recid(DumpLog).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND DumpLog WHERE recid(DumpLog) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DumpLog THEN DO:
           Memory = recid(DumpLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE DumpLog THEN Memory = recid(DumpLog).
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
           FIND DumpLog WHERE recid(DumpLog) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY liDumpID WITH FRAME F1.

       UPDATE liDumpID WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF liDumpID > 0 THEN DO:

          FIND FIRST DumpLog USE-INDEX DumpId WHERE 
                     DumpLog.DumpId >= liDumpID 
          NO-LOCK NO-ERROR.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDumpLog).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 5. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDumpLog).

       RUN local-disp-row.
       xrecid = recid(DumpLog).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(DumpLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(DumpLog) must-print = TRUE.
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
      FIND DumpLog WHERE recid(DumpLog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND DumpLog WHERE recid(DumpLog) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.


PROCEDURE LOCAL-FIND-FIRST:

IF iiDumpID > 0 THEN DO:
      FIND FIRST DumpLog USE-INDEX DumpId WHERE
                 DumpLog.DumpId = iiDumpID NO-LOCK NO-ERROR.
   END.
   ELSE
      FIND FIRST DumpLog USE-INDEX CreateStart WHERE
                 DumpLog.CreateStart <= fMakeTS().

END PROCEDURE.



PROCEDURE LOCAL-FIND-LAST:

   IF iiDumpID > 0 THEN DO:
      FIND LAST DumpLog USE-INDEX DumpId WHERE
                DumpLog.DumpId = iiDumpID NO-LOCK NO-ERROR.
   END.
   ELSE
      FIND LAST DumpLog USE-INDEX CreateStart WHERE
                DumpLog.CreateStart <= fMakeTS().
 
END PROCEDURE.



PROCEDURE LOCAL-FIND-NEXT:

   IF iiDumpID > 0 THEN DO:
      FIND NEXT DumpLog USE-INDEX DumpId WHERE
                 DumpLog.DumpId = iiDumpID NO-LOCK NO-ERROR.
   END.
   ELSE
      FIND NEXT DumpLog USE-INDEX CreateStart WHERE
                DumpLog.CreateStart <= fMakeTS().
 
END PROCEDURE.



PROCEDURE LOCAL-FIND-PREV:

   IF iiDumpID > 0 THEN DO:
      FIND PREV DumpLog USE-INDEX DumpId WHERE
                 DumpLog.DumpId = iiDumpID NO-LOCK NO-ERROR.
   END.
   ELSE
      FIND PREV DumpLog USE-INDEX CreateStart WHERE
                DumpLog.CreateStart <= fMakeTS().
 
END PROCEDURE.



PROCEDURE LOCAL-DISP-ROW:

       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       DumpLog.DumpId 
       DumpLog.DumpLogId 
       DumpLog.CreateStart 
       DumpLog.DumpType
       DumpLog.DumpLogStatus
       DumpLog.FileName
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

    fSplitTS(DumpLog.CreateStart,
             OUTPUT ldtDate,
             OUTPUT liTime).


END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR liInfoPos AS INT  NO-UNDO.
   DEF VAR liLine    AS INT  NO-UNDO.

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      ASSIGN
         lcTime = STRING(ldtDate,"99-99-99") + " " + STRING(liTime,"hh:mm:ss")
         lcInfo = ""
         lcCreateStart = (IF DumpLog.CreateStart > 0
                          THEN fTS2HMS(DumpLog.CreateStart)
                          ELSE "")
         lcCreateEnd = (IF DumpLog.CreateEnd > 0
                        THEN fTS2HMS(DumpLog.CreateEnd)
                        ELSE "").
      
      DISP 
          DumpLog.DumpId
          DumpLog.DumpLogId
          DumpLog.CreateStart lcCreateStart
          DumpLog.CreateEnd lcCreateEnd
          DumpLog.DumpType
          DumpLog.DumpLogStatus
          DumpLog.FileName
          DumpLog.Filesize
      WITH FRAME lis.

      fStatusName(DumpLog.DumpLogStatus).
        
      ASSIGN 
         ehto = 0
         ufk  = 0
         ufk[1] = 7 WHEN lcRight = "RW"
      /* ufk[4] = 1697 */
         ufk[8] = 8.
      RUN Syst/ufkey.p.
      
      IF toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
         ehto = 9.
         RUN Syst/ufkey.p.
         
         FIND CURRENT DumpLog EXCLUSIVE-LOCK.
         UPDATE DumpLog.DumpLogStatus WITH FRAME lis EDITING:
         
            READKEY.
         
            nap = KEYLABEL(LASTKEY).
            
            IF nap = "F9" AND FRAME-FIELD = "DumpLogStatus" THEN DO:

               RUN Help/h-tmscodes.p(INPUT "ActionLog",  /* TableName*/
                                    "ActionStatus", /* FieldName */
                                    "Log", /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ?
               THEN DO WITH FRAME lis:
                  DISPLAY INTEGER(lcCode) ;& DumpLog.DumpLogStatus.
               END.

               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.
 
            ELSE IF LOOKUP(nap,poisnap) > 0 THEN DO:
            
               IF FRAME-FIELD = "DumpLogStatus" THEN DO:
                  fStatusName(INPUT INPUT DumpLog.DumpLogStatus).

                  IF lcStatus = "" THEN DO:
                     MESSAGE "Unknown status"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
            END.
            
            APPLY LASTKEY.
         END.
         
         LEAVE.
      END.

      ELSE IF toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.
