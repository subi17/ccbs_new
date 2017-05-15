/* ----------------------------------------------------------------------
  MODULE .......: ActionLog
  TASK .........: browse table ActionLog
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 19.04.07
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable ActionLog

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ActionLog'}
{Func/timestamp.i}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhActionLog AS HANDLE NO-UNDO.
   lhActionLog = BUFFER ActionLog:HANDLE.
   RUN StarEventInitialize(lhActionLog).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhActionLog).
   END.

END.

DEF INPUT PARAMETER icTableName AS CHAR NO-UNDO.
DEF INPUT PARAMETER icKeyValue  AS CHAR NO-UNDO. 
DEF INPUT PARAMETER icActionID  AS CHAR NO-UNDO. 

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcTable      LIKE ActionLog.TableName   NO-UNDO.
DEF VAR lcKey        LIKE ActionLog.KeyValue    NO-UNDO.
DEF VAR lcActionID   LIKE ActionLog.ActionID    NO-UNDO. 

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

DEF VAR lcTime       AS CHAR NO-UNDO.
DEF VAR ldtDate      AS DATE NO-UNDO.
DEF VAR liTime       AS INT  NO-UNDO. 
DEF VAR lcInfo       AS CHAR NO-UNDO.
DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcCustName   AS CHAR NO-UNDO.
DEF VAR lcCode       AS CHAR NO-UNDO.
DEF VAR lcLargeInfo  AS CHAR NO-UNDO.

form
    ActionLog.ActionID  FORMAT "X(13)" COLUMN-LABEL "Action ID"
    ActionLog.TableName FORMAT "X(12)"
    ActionLog.KeyValue  FORMAT "X(15)"                    
    ldtDate             FORMAT "99-99-99" COLUMN-LABEL "Date"
    ActionLog.ActionStatus  COLUMN-LABEL "St"
    ActionLog.ActionChar FORMAT "X(22)" COLUMN-LABEL "Info"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " ACTION LOG "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ActionLog.ActionID    COLON 16 FORMAT "X(30)" 
    ActionLog.TableName   COLON 16 FORMAT "X(30)"
    ActionLog.KeyValue    COLON 16 FORMAT "X(50)"
    ActionLog.CustNum     COLON 16 
      lcCustName NO-LABEL FORMAT "X(35)" SKIP
    lcTime                COLON 16 LABEL "Time" FORMAT "X(20)" 
      "(" SPACE(0) 
      ActionLog.ActionTS NO-LABEL
      SPACE(0) ")" SKIP
    ActionLog.UserCode     COLON 16   
    ActionLog.ActionStatus COLON 16 
      lcStatus NO-LABEL FORMAT "X(30)" 
    ActionLog.ActionPeriod COLON 16 
    ActionLog.FromDate COLON 16 LABEL "Dates"
      "-"
      ActionLog.ToDate NO-LABEL SKIP
    ActionLog.ActionDec    COLON 16
    lcInfo                 COLON 16
       LABEL "Info"
       FORMAT "X(30)"
    ActionLog.ActionChar COLON 2
       NO-LABEL
       VIEW-AS EDITOR SIZE 70 BY 6
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
   
{Func/brand.i}

form /* seek  ActionLog */
    "Brand :" lcBrand skip
    "Action:" lcActionID FORMAT "X(20)"
    HELP "Enter action ID "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Action ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


form /* seek  ActionLog */
    "Brand:" lcBrand skip
    "Table:" lcTable FORMAT "X(30)"
       HELP "Enter table name " SKIP 
    "Key .:" lcKey FORMAT "X(30)"
       HELP "Enter key value for table"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Table "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FUNCTION fStatusName RETURNS LOGICAL
   (INPUT iiStatus AS INT):
   
   lcStatus = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "ActionLog",
                               "ActionStatus",
                               STRING(iiStatus)).
                               
   DISP lcStatus WITH FRAME lis.
END.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

IF icTableName > "" THEN 
   icActionID = "".

IF icTableName > "" OR icActionID > "" THEN MaxOrder = 1.

RUN local-find-first.

IF AVAILABLE ActionLog THEN ASSIGN
   Memory       = recid(ActionLog)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No error logs available !" VIEW-AS ALERT-BOX.
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
        FIND ActionLog WHERE recid(ActionLog) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ActionLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ActionLog).
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
        ufk[1] = 1124  
        ufk[2] = 2121
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        IF icTableName > "" THEN ASSIGN 
           ufk[1] = 0
           ufk[2] = 0
           ufk[3] = 0.
        ELSE IF icActionID > "" THEN ASSIGN 
           ufk[1] = 0
           ufk[3] = 0.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ActionLog.ActionID {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ActionLog.ActionID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ActionLog.TableName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ActionLog.TableName WITH FRAME sel.
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
        FIND ActionLog WHERE recid(ActionLog) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ActionLog THEN
              ASSIGN FIRSTrow = i Memory = recid(ActionLog).
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
           IF NOT AVAILABLE ActionLog THEN DO:
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
                rtab[1] = recid(ActionLog)
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
           IF NOT AVAILABLE ActionLog THEN DO:
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
              rtab[FRAME-DOWN] = recid(ActionLog).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ActionLog WHERE recid(ActionLog) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ActionLog THEN DO:
           Memory = recid(ActionLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ActionLog THEN Memory = recid(ActionLog).
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
           FIND ActionLog WHERE recid(ActionLog) = Memory NO-LOCK.
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
       DISPLAY lcBrand WITH FRAME F1.

       UPDATE lcBrand WHEN gcAllBrand
              lcActionID WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcActionID > "" THEN DO:

          FIND FIRST ActionLog USE-INDEX ActionID WHERE 
                     ActionLog.Brand     = lcBrand    AND
                     ActionLog.ActionID >= lcActionID 
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       
       UPDATE lcBrand WHEN gcAllBrand
              lcTable 
              lcKey
       WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF lcTable > "" THEN DO:

          IF icActionID > "" THEN 
             FIND FIRST ActionLog USE-INDEX TableName WHERE 
                        ActionLog.Brand     = lcBrand    AND
                        ActionLog.TableName = lcTable    AND
                        ActionLog.KeyValue >= lcKey      AND
                        ActionLog.ActionID  = icActionID 
             NO-LOCK NO-ERROR.

          ELSE            
             FIND FIRST ActionLog USE-INDEX TableName WHERE 
                        ActionLog.Brand     = lcBrand    AND
                        ActionLog.TableName = lcTable    AND
                        ActionLog.KeyValue >= lcKey     
             NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" AND ufk[6] > 0 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ActionLog.TableName ActionLog.KeyValue ldtDate.

       RUN local-find-NEXT.
       IF AVAILABLE ActionLog THEN Memory = recid(ActionLog).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ActionLog THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ActionLog).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ActionLog.TableName ActionLog.KeyValue ldtDate.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhActionLog).

           DELETE ActionLog.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ActionLog THEN DO:
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
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhActionLog).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 5. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ActionLog.TableName.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhActionLog).

       RUN local-disp-row.
       xrecid = recid(ActionLog).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ActionLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ActionLog) must-print = TRUE.
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
      FIND ActionLog WHERE recid(ActionLog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ActionLog WHERE recid(ActionLog) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN DO:

      IF icTableName > "" THEN DO:
   
         IF icKeyValue > "" THEN 
            FIND FIRST ActionLog USE-INDEX TableName WHERE
                       ActionLog.Brand     = gcBrand     AND
                       ActionLog.TableName = icTableName AND
                       ActionLog.KeyValue  = icKeyValue NO-LOCK NO-ERROR.      

         ELSE 
            FIND FIRST ActionLog USE-INDEX TableName WHERE
                       ActionLog.Brand     = gcBrand     AND
                       ActionLog.TableName = icTableName NO-LOCK NO-ERROR.
      END.                 

      ELSE IF icActionID > "" THEN 
         FIND FIRST ActionLog USE-INDEX ActionID WHERE
                    ActionLog.Brand    = gcBrand AND
                    ActionLog.ActionID = icActionID NO-LOCK NO-ERROR.
                               
      ELSE FIND FIRST ActionLog USE-INDEX ActionID WHERE
                      ActionLog.Brand = gcBrand NO-LOCK NO-ERROR.
   END.      

   ELSE IF order = 2 THEN DO:
      FIND FIRST ActionLog USE-INDEX TableName WHERE
                 ActionLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN DO:

      IF icTableName > "" THEN DO:
   
         IF icKeyValue > "" THEN 
            FIND LAST ActionLog USE-INDEX TableName WHERE
                      ActionLog.Brand     = gcBrand     AND
                      ActionLog.TableName = icTableName AND
                      ActionLog.KeyValue  = icKeyValue NO-LOCK NO-ERROR.      

         ELSE 
            FIND LAST ActionLog USE-INDEX TableName WHERE
                      ActionLog.Brand     = gcBrand     AND
                      ActionLog.TableName = icTableName NO-LOCK NO-ERROR.
      END.                 

      ELSE IF icActionID > "" THEN 
         FIND LAST ActionLog USE-INDEX ActionID WHERE
                   ActionLog.Brand    = gcBrand AND
                   ActionLog.ActionID = icActionID NO-LOCK NO-ERROR.
                               
      ELSE FIND LAST ActionLog USE-INDEX ActionID WHERE
                     ActionLog.Brand = gcBrand NO-LOCK NO-ERROR.
   END.      

   ELSE IF order = 2 THEN DO:
      FIND LAST ActionLog USE-INDEX TableName WHERE
                ActionLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN DO:

      IF icTableName > "" THEN DO:
   
         IF icKeyValue > "" THEN 
            FIND NEXT ActionLog USE-INDEX TableName WHERE
                      ActionLog.Brand     = gcBrand     AND
                      ActionLog.TableName = icTableName AND
                      ActionLog.KeyValue  = icKeyValue NO-LOCK NO-ERROR.      

         ELSE 
            FIND NEXT ActionLog USE-INDEX TableName WHERE
                      ActionLog.Brand     = gcBrand     AND
                      ActionLog.TableName = icTableName NO-LOCK NO-ERROR.
      END.                 

      ELSE IF icActionID > "" THEN 
         FIND NEXT ActionLog USE-INDEX ActionID WHERE
                   ActionLog.Brand    = gcBrand AND
                   ActionLog.ActionID = icActionID NO-LOCK NO-ERROR.
                               
      ELSE FIND NEXT ActionLog USE-INDEX ActionID WHERE
                     ActionLog.Brand = gcBrand NO-LOCK NO-ERROR.
   END.      
 
   ELSE IF order = 2 THEN DO:
      FIND NEXT ActionLog USE-INDEX TableName WHERE
                ActionLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN DO:

      IF icTableName > "" THEN DO:
   
         IF icKeyValue > "" THEN 
            FIND PREV ActionLog USE-INDEX TableName WHERE
                      ActionLog.Brand     = gcBrand     AND
                      ActionLog.TableName = icTableName AND
                      ActionLog.KeyValue  = icKeyValue NO-LOCK NO-ERROR.      

         ELSE 
            FIND PREV ActionLog USE-INDEX TableName WHERE
                      ActionLog.Brand     = gcBrand     AND
                      ActionLog.TableName = icTableName NO-LOCK NO-ERROR.
      END.                 

      ELSE IF icActionID > "" THEN 
         FIND PREV ActionLog USE-INDEX ActionID WHERE
                   ActionLog.Brand    = gcBrand AND
                   ActionLog.ActionID = icActionID NO-LOCK NO-ERROR.
                               
      ELSE FIND PREV ActionLog USE-INDEX ActionID WHERE
                     ActionLog.Brand = gcBrand NO-LOCK NO-ERROR.
   END.      

   ELSE IF order = 2 THEN DO:
      FIND PREV ActionLog USE-INDEX TableName WHERE
                ActionLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       ActionLog.ActionID 
       ActionLog.TableName 
       ActionLog.KeyValue
       ActionLog.ActionStatus
       ldtDate
       ActionLog.ActionChar
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

    fSplitTS(ActionLog.ActionTS,
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
         lcInfo = "".
      
      CASE ActionLog.ActionID:
      WHEN "MINCONS" THEN DO:
         FIND Invoice WHERE Invoice.InvNum = INTEGER(ActionLog.ActionDec)
            NO-LOCK NO-ERROR.
         IF AVAILABLE Invoice THEN lcInfo = Invoice.ExtInvID.
      END.
      END CASE.
      
      lcCustName = "".
      IF ActionLog.CustNum > 0 THEN DO:
         FIND Customer WHERE Customer.CustNum = ActionLog.CustNum 
            NO-LOCK NO-ERROR.
         IF AVAILABLE Customer THEN 
            lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                          BUFFER Customer).
      END.
      
      DISP 
           ActionLog.ActionID
           ActionLog.TableName
           ActionLog.KeyValue
           ActionLog.CustNum
           lcCustName
           lcTime
           ActionLog.ActionTS
           ActionLog.UserCode
           ActionLog.ActionStatus
           ActionLog.ActionPeriod
           ActionLog.FromDate
           ActionLog.ToDate
           ActionLog.ActionDec
           ActionLog.ActionChar
           lcInfo
      WITH FRAME lis.

      fStatusName(ActionLog.ActionStatus).
        
      ASSIGN 
         ehto = 0
         ufk  = 0
         ufk[1] = 7 WHEN lcRight = "RW"
         ufk[4] = 1697
         ufk[8] = 8.
      RUN Syst/ufkey.p.
      
      IF toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
         ehto = 9.
         RUN Syst/ufkey.p.
         
         FIND CURRENT ActionLog EXCLUSIVE-LOCK.
         UPDATE ActionLog.ActionStatus WITH FRAME lis EDITING:
         
            READKEY.
         
            nap = KEYLABEL(LASTKEY).
            
            IF nap = "F9" AND FRAME-FIELD = "ActionStatus" THEN DO:

               RUN Help/h-tmscodes.p(INPUT "ActionLog",  /* TableName*/
                                    "ActionStatus", /* FieldName */
                                    "Log", /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ?
               THEN DO WITH FRAME lis:
                  DISPLAY INTEGER(lcCode) ;& ActionLog.ActionStatus.
               END.

               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.
 
            ELSE IF LOOKUP(nap,poisnap) > 0 THEN DO:
            
               IF FRAME-FIELD = "ActionStatus" THEN DO:
                  fStatusName(INPUT INPUT ActionLog.ActionStatus).

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

      ELSE IF toimi = 4 THEN DO:
        
         liInfoPos = 1.

         REPEAT WITH FRAME fInfo:
            ASSIGN
               lcLargeInfo = ""
               liLine      = 0.
               
            /* note; this does not work properly if there are long lines that
               are automatically wrapped */
            DO i = liInfoPos TO NUM-ENTRIES(ActionLog.ActionChar,CHR(10)):
               lcLargeInfo = lcLargeInfo + 
                             ENTRY(i,ActionLog.ActionChar,CHR(10)) +
                             CHR(10).
               liLine = liLine + 1.
               IF liLine >= 17 THEN LEAVE.
            END.

            liInfoPos = i. 

            PAUSE 0.
            DISP lcLargeInfo WITH FRAME fInfo.
         
            ASSIGN 
               ufkey = TRUE
               ehto  = 0
               ufk   = 0
               ufk[8] = 8.
 
            IF liInfoPos < NUM-ENTRIES(ActionLog.ActionChar,CHR(10)) 
            THEN ufk[4] = 20.
 
            RUN Syst/ufkey.p.
        
            IF toimi = 4 THEN NEXT. 
            
            ELSE IF toimi = 8 THEN DO:
               HIDE FRAME fInfo NO-PAUSE.
               LEAVE.
            END.
         END.   
      END.

      ELSE IF toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.

