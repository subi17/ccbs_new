/* ----------------------------------------------------------------------
  MODULE .......: EVENTSEL.P
  TASK .........: Browse Eventlog
  APPLICATION ..: TMS
  AUTHOR .......: jr
  CREATED ......: 23-04-02
  CHANGED ......: 17.09.02/jr Viewer frame change and functionkeys
                  23.09.02/jr Find for table & key
                  23.09.02/jr Enter & F5 use same viewer
                  23.09.02/jr removed unused parts and cleaning
                  24.09.02/jr added indexes with date and table
                  25.09.02/aam changes to finds (f2 & f3),
                               bigger frame,
                               show " / " instead of chr(255) in Key
                  06.11.02/jr Added Date to User search               
                  06.03.03/tk tokens             
                  15.04.03/aam better use of index to F3
                  13.07.04/tk  replace key " / " to chr(255) in F3 
                  26.10.04/aam EventLogStatus to browser,
                               parameters for eventview changed
                  14.04.05/aam input icTableName and icKey
                  08.05.06/aam smaller frame when icTableName given,
                               #BEGIN in icKey
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} /*upd = TRUE.*/
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Eventlog'}
{Func/lib/accesslog.i}
{Syst/tmsconst.i}

&GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

{Func/lib/eventlog.i}

DEF INPUT PARAMETER icTableName AS CHAR NO-UNDO. 
DEF INPUT PARAMETER icKey       AS CHAR NO-UNDO. 

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR evdate   LIKE Eventlog.EventDate       NO-UNDO. 
DEF VAR ldate    LIKE Eventlog.EventDate       NO-UNDO.
DEF VAR UserCode LIKE Eventlog.UserCode        NO-UNDO.
DEF VAR liType   LIKE EventLog.EventLogStatus  NO-UNDO.
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
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcTable      AS CHAR FORMAT "X(20)"    NO-UNDO.
DEF VAR lcKey        AS CHAR FORMAT "X(20)"    NO-UNDO.
DEF VAR lcevtime     AS CHAR FORMAT "XX:XX:XX" NO-UNDO.
DEF VAR llBegins     AS LOG                    NO-UNDO.
DEF VAR lcProgram    AS CHAR                   NO-UNDO.

DEFINE VARIABLE muutokset AS CHARACTER NO-UNDO.

lcProgram = PROGRAM-NAME(1).

form
    Eventlog.EventDate 
    Eventlog.EventTime 
    Eventlog.Action         /* COLUMN-LABEL FORMAT */
    Eventlog.UserCode
    Eventlog.TableName   FORMAT "X(13)"    
    Eventlog.Key         format "x(23)" column-label "Key"
    EventLog.EventLogStatus FORMAT ">9" COLUMN-LABEL "Type"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
    " Eventlog BROWSER "
    + string(TODAY,"99-99-99") + " "
    FRAME sel.

form
    Eventlog.TableName     /* LABEL FORMAT */
    Eventlog.EventDate
    Eventlog.EventTime  
    Eventlog.Action        /* LABEL FORMAT */
    EventLog.EventLogStatus LABEL "Type" 
    Eventlog.UserCode
    Eventlog.Key      
    muutokset VIEW-AS EDITOR size-chars 60 BY 10
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    2 columns
    FRAME lis.

form /* seek Eventlog  BY  Date */
    "Date..:" evdate
    HELP "Enter Date" SKIP
    "Time..:"
    lcevtime
    HELP "Enter Time 99:99:99"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Date & Time"
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Eventlog  BY UserCode */
    "User.:" UserCode
    HELP "Enter Usercode" SKIP
    "Date.:" ldate
    HELP "Enter Date"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND User "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek Eventlog  BY  TableName and keyvalue */
    "Table..:" lcTable HELP "Enter TableName or beginning of it " SKIP
    "Key....:" lcKey   HELP "Enter KeyValue or beginning of it"
            WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Table & Key "
                COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f3.

IF icTableName > "" 
THEN ASSIGN MaxOrder = 1    
            FrmRow   = 2
            FrmDown  = 13.

/* only beginning of key needs to match */
IF ENTRY(1,icKey,CHR(255)) = "#BEGIN" THEN ASSIGN
   icKey    = SUBSTRING(icKey,8)
   llBegins = TRUE.
ELSE llBegins = FALSE.
   
Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = "  By Date  ,  By User  ,  By Table ".

RUN local-find-first.
IF AVAILABLE Eventlog THEN ASSIGN
   memory       = recid(Eventlog)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print   = FALSE
   must-add     = TRUE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Eventlog WHERE recid(Eventlog) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Eventlog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Eventlog).
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
        Syst.Var:ufk[1]= 28  Syst.Var:ufk[2]= 542 Syst.Var:ufk[3]= 2121 Syst.Var:ufk[4]= 0
        Syst.Var:ufk[5]=265 Syst.Var:ufk[6] = 0
        Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
        
        IF icTableName > "" THEN ASSIGN
           Syst.Var:ufk[1] = 0 
           Syst.Var:ufk[2] = 0
           Syst.Var:ufk[3] = 0.
           
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Eventlog.EventDate {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) Eventlog.EventDate WITH FRAME sel.
      END.
      ELSE  IF order = 2 THEN DO:
        CHOOSE ROW Eventlog.UserCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) Eventlog.UserCode WITH FRAME sel.
      END.
      IF order = 3 THEN DO:
        CHOOSE ROW Eventlog.TableName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) Eventlog.TableName WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND Eventlog WHERE recid(Eventlog) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Eventlog THEN
              ASSIGN FIRSTrow = i memory = recid(Eventlog).
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

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE Eventlog THEN DO:
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
                rtab[1] = recid(Eventlog)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE Eventlog THEN DO:
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
              rtab[FRAME-DOWN] = recid(Eventlog).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Eventlog WHERE recid(Eventlog) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Eventlog THEN DO:
           memory = recid(Eventlog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Eventlog THEN memory = recid(Eventlog).
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
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND Eventlog WHERE recid(Eventlog) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */                          
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND Syst.Var:ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       ASSIGN
             lcevtime = "".
       SET evdate 
           lcevtime WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF evdate ENTERED THEN DO:
          IF lcevtime = "" 
          THEN DO:
              FIND FIRST EventLog NO-LOCK WHERE
                         EventLog.EventDate = evdate NO-ERROR.
              IF NOT AVAILABLE EventLog THEN 
                 FIND LAST EventLog NO-LOCK WHERE
                            EventLog.EventDate >= evdate NO-ERROR.
          END. 

          ELSE DO:
             FIND LAST Eventlog NO-LOCK WHERE 
                       Eventlog.EventDate = evdate AND
                       Eventlog.EventTime >= STRING(lcevtime,"99:99:99")
                NO-ERROR.
             IF NOT AVAILABLE EventLog THEN 
                FIND FIRST EventLog NO-LOCK WHERE 
                           EventLog.EventDate = evdate NO-ERROR.
             IF NOT AVAILABLE EventLog THEN
                FIND LAST EventLog NO-LOCK WHERE
                          EventLog.EventDate >= evdate NO-ERROR.
          END. 

          IF NOT AVAILABLE Eventlog THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some eventlog/tablename was found */
          ASSIGN order = 1 memory = recid(Eventlog) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 AND Syst.Var:ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       SET UserCode 
           ldate
       WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF UserCode ENTERED THEN DO:
          FIND FIRST Eventlog
             WHERE Eventlog.UserCode >= UserCode
             AND (IF ldate entered THEN  
                  Eventlog.EventDate = ldate
                  ELSE TRUE)
          USE-INDEX UserName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Eventlog THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some eventlog/usercode was found */
          ASSIGN order = 2 memory = recid(Eventlog) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY column 3 */
     ELSE IF LOOKUP(Syst.Var:nap,"3,f3") > 0 AND Syst.Var:ufk[3] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f3.
       ASSIGN
             lcTable = ""
             lcKey   = "".

       SET lcTable lcKey WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF lcTable ENTERED OR lcKey ENTERED THEN
       DO:
          IF lcKey NE "" THEN DO:
             FIND FIRST Eventlog NO-LOCK WHERE 
                        Eventlog.TableName = lcTable AND
                        EventLog.Key BEGINS replace(lcKey," / ",chr(255))
             NO-ERROR.
             IF LOOKUP(lcTable, {&ACCESSLOG_TABLES} ) > 0  THEN
                RUN CreateReadAccess(lcTable, Syst.Var:katun, EventLog.Key, lcProgram, "Key" ).
          END.  
          ELSE 
          FIND FIRST Eventlog NO-LOCK WHERE 
                     Eventlog.TableName = lcTable NO-ERROR.
          IF NOT AVAILABLE Eventlog THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some eventlog/tablename was found */
          ASSIGN 
                order      = 3 
                memory     = recid(Eventlog) 
                must-print = TRUE.
          NEXT LOOP. 
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5,enter,return") > 0 THEN DO:

        RUN local-find-this (FALSE).

        IF Eventlog.eventlogstatus = 0 OR EventLog.EventLogStatus > 2 
        THEN RUN Mc/eventview.p (RECID(Eventlog)).

        ELSE RUN Mc/eventview3.p(INPUT Eventlog.TableName,
                            INPUT Eventlog.DataValues,
                            INPUT Eventlog.EventDate,
                            INPUT Eventlog.Eventtime,
                            INPUT RECID(EventLOG)).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(Eventlog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(Eventlog) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND Eventlog WHERE recid(Eventlog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Eventlog WHERE recid(Eventlog) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icTableName > "" THEN DO:
   
       IF llBegins THEN 
       FIND FIRST EventLog WHERE
                  EventLog.TableName = icTableName AND
                  EventLog.Key  BEGINS icKey NO-LOCK NO-ERROR.
        
       ELSE 
       FIND FIRST EventLog WHERE
                  EventLog.TableName = icTableName AND
                  EventLog.Key       = icKey NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND FIRST Eventlog
       /* srule */ USE-INDEX eventDate NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST Eventlog 
          USE-INDEX UserName
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST Eventlog USE-INDEX TableName
       /* srule */ NO-LOCK NO-ERROR.
   END. 
       
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF icTableName > "" THEN DO:
       IF llBegins THEN 
       FIND LAST EventLog WHERE
                 EventLog.TableName = icTableName AND
                 EventLog.Key  BEGINS icKey NO-LOCK NO-ERROR.
        
       ELSE 
       FIND LAST EventLog WHERE
                 EventLog.TableName = icTableName AND
                 EventLog.Key       = icKey NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND LAST Eventlog
       /* srule */ USE-INDEX eventDate NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST Eventlog 
          USE-INDEX UserName
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST Eventlog USE-INDEX TableName
       /* srule */ NO-LOCK NO-ERROR.
   END. 
 
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF icTableName > "" THEN DO:
       IF llBegins THEN 
       FIND NEXT EventLog WHERE
                 EventLog.TableName = icTableName AND
                 EventLog.Key  BEGINS icKey NO-LOCK NO-ERROR.
        
       ELSE 
       FIND NEXT EventLog WHERE
                 EventLog.TableName = icTableName AND
                 EventLog.Key       = icKey NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND NEXT Eventlog
       /* srule */ USE-INDEX eventDate NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT Eventlog 
          USE-INDEX UserName
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT Eventlog USE-INDEX TableName
       /* srule */ NO-LOCK NO-ERROR.
   END. 
 
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF icTableName > "" THEN DO:
       IF llBegins THEN 
       FIND PREV EventLog WHERE
                 EventLog.TableName = icTableName AND
                 EventLog.Key  BEGINS icKey NO-LOCK NO-ERROR.
        
       ELSE 
       FIND PREV EventLog WHERE
                 EventLog.TableName = icTableName AND
                 EventLog.Key       = icKey NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN FIND PREV Eventlog
       /* srule */ USE-INDEX eventDate NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV Eventlog 
          USE-INDEX UserName
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV Eventlog USE-INDEX TableName
       /* srule */ NO-LOCK NO-ERROR.
   END. 
 END PROCEDURE.

PROCEDURE local-disp-row:

       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       Eventlog.TableName  
       Eventlog.EventDate
       Eventlog.EventTime    
       Eventlog.Action
       Eventlog.UserCode
       REPLACE(Eventlog.Key,CHR(255)," / ") ;& EventLog.Key
       EventLog.EventLogStatus
       WITH FRAME sel.
END PROCEDURE.

