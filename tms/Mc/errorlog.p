/* ----------------------------------------------------------------------
  MODULE .......: errorlog
  TASK .........: browse table ErrorLog
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 19.04.07
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable ErrorLog

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ErrorLog'}
{Func/timestamp.i}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhErrorLog AS HANDLE NO-UNDO.
   lhErrorLog = BUFFER ErrorLog:HANDLE.
   RUN StarEventInitialize(lhErrorLog).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhErrorLog).
   END.

END.

DEF INPUT PARAMETER icTableName AS CHAR NO-UNDO.
DEF INPUT PARAMETER icKeyValue  AS CHAR NO-UNDO. 
DEF INPUT PARAMETER icActionID  AS CHAR NO-UNDO. 

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcTable      LIKE ErrorLog.TableName   NO-UNDO.
DEF VAR lcKey        LIKE ErrorLog.KeyValue    NO-UNDO.
DEF VAR lcActionID   LIKE ErrorLog.ActionID    NO-UNDO. 
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

DEF VAR lcTime    AS CHAR NO-UNDO.
DEF VAR ldtDate   AS DATE NO-UNDO.
DEF VAR liTime    AS INT  NO-UNDO. 

form
    ErrorLog.ActionID  FORMAT "X(12)"   COLUMN-LABEL "Action ID"
    ErrorLog.TableName FORMAT "X(12)"
    ErrorLog.KeyValue  FORMAT "X(10)"                    
    ldtDate            FORMAT "99-99-99" COLUMN-LABEL "Date"
    ErrorLog.ErrorMsg  FORMAT "X(31)"    COLUMN-LABEL "Error"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " ERROR LOG "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ErrorLog.ActionID     COLON 16 FORMAT "X(16)"
    ErrorLog.TableName    COLON 16   
    ErrorLog.KeyValue     COLON 16
    lcTime                COLON 16 LABEL "Time" FORMAT "X(20)" 
      "(" SPACE(0) 
      ErrorLog.ActionTS NO-LABEL
      SPACE(0) ")" SKIP
    ErrorLog.UserCode   COLON 16 FORMAT "X(16)"
    ErrorLog.ErrorCode  COLON 16 
    ErrorLog.ErrorChar  COLON 16 FORMAT "X(30)"
    ErrorLog.ErrorMsg   COLON 16 
       VIEW-AS EDITOR SIZE 60 BY 5
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

{Func/brand.i}

form /* seek  ErrorLog */
    "Brand :" lcBrand skip
    "Action:" lcActionID
    HELP "Enter action ID "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Action ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


form /* seek  ErrorLog */
    "Brand:" lcBrand skip
    "Table:" lcTable
       HELP "Enter table name "
    "Key .:" lcKey
       HELP "Enter key value for table"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Table "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


IF icTableName > "" THEN 
   icActionID = "".

IF icTableName > "" OR icActionID > "" THEN ASSIGN
 MaxOrder = 1
 FrmRow   = 3
 FrmDown  = 13.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-find-first.

IF AVAILABLE ErrorLog THEN ASSIGN
   Memory       = recid(ErrorLog)
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
        FIND ErrorLog WHERE recid(ErrorLog) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ErrorLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ErrorLog).
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
        ufk   = 0
        ufk[1]= 1124  
        ufk[2]= 2121
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.

        IF icTableName > "" THEN ASSIGN 
           ufk[1] = 0
           ufk[2] = 0.
        ELSE IF icActionID > "" THEN 
           ufk[1] = 0.

        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ErrorLog.ActionID ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ErrorLog.ActionID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ErrorLog.TableName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ErrorLog.TableName WITH FRAME sel.
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
        FIND ErrorLog WHERE recid(ErrorLog) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ErrorLog THEN
              ASSIGN FIRSTrow = i Memory = recid(ErrorLog).
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
           IF NOT AVAILABLE ErrorLog THEN DO:
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
                rtab[1] = recid(ErrorLog)
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
           IF NOT AVAILABLE ErrorLog THEN DO:
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
              rtab[FRAME-DOWN] = recid(ErrorLog).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ErrorLog WHERE recid(ErrorLog) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ErrorLog THEN DO:
           Memory = recid(ErrorLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ErrorLog THEN Memory = recid(ErrorLog).
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
           FIND ErrorLog WHERE recid(ErrorLog) = Memory NO-LOCK.
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
       DISPLAY lcBrand WITH FRAME F1.

       UPDATE lcBrand WHEN gcAllBrand
              lcActionID WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcActionID > "" THEN DO:

          FIND FIRST ErrorLog USE-INDEX ActionID WHERE 
                     ErrorLog.Brand     = lcBrand    AND
                     ErrorLog.ActionID >= lcActionID 
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       
       UPDATE lcBrand WHEN gcAllBrand
              lcTable 
              lcKey
       WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF lcTable > "" THEN DO:

          IF icActionID > "" THEN 
             FIND FIRST ErrorLog USE-INDEX TableName WHERE 
                        ErrorLog.Brand     = lcBrand    AND
                        ErrorLog.TableName = lcTable    AND
                        ErrorLog.KeyValue >= lcKey      AND
                        ErrorLog.ActionID  = icActionID 
             NO-LOCK NO-ERROR.

          ELSE            
             FIND FIRST ErrorLog USE-INDEX TableName WHERE 
                        ErrorLog.Brand     = lcBrand    AND
                        ErrorLog.TableName = lcTable    AND
                        ErrorLog.KeyValue >= lcKey     
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
       ErrorLog.TableName ErrorLog.KeyValue ldtDate.

       RUN local-find-NEXT.
       IF AVAILABLE ErrorLog THEN Memory = recid(ErrorLog).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ErrorLog THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ErrorLog).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ErrorLog.TableName ErrorLog.KeyValue ldtDate.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhErrorLog).

           DELETE ErrorLog.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ErrorLog THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhErrorLog).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 5. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ErrorLog.TableName.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhErrorLog).

       RUN local-disp-row.
       xrecid = recid(ErrorLog).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ErrorLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ErrorLog) must-print = TRUE.
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
      FIND ErrorLog WHERE recid(ErrorLog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ErrorLog WHERE recid(ErrorLog) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN DO:

      IF icTableName > "" THEN DO:
   
         IF icKeyValue > "" THEN 
            FIND FIRST ErrorLog USE-INDEX TableName WHERE
                       ErrorLog.Brand     = gcBrand     AND
                       ErrorLog.TableName = icTableName AND
                       ErrorLog.KeyValue  = icKeyValue NO-LOCK NO-ERROR.      

         ELSE 
            FIND FIRST ErrorLog USE-INDEX TableName WHERE
                       ErrorLog.Brand     = gcBrand     AND
                       ErrorLog.TableName = icTableName NO-LOCK NO-ERROR.
      END.                 

      ELSE IF icActionID > "" THEN 
         FIND FIRST ErrorLog USE-INDEX ActionID WHERE
                    ErrorLog.Brand    = gcBrand AND
                    ErrorLog.ActionID = icActionID NO-LOCK NO-ERROR.
                               
      ELSE FIND FIRST ErrorLog USE-INDEX ActionID WHERE
                      ErrorLog.Brand = gcBrand NO-LOCK NO-ERROR.
   END.      

   ELSE IF order = 2 THEN DO:
      FIND FIRST ErrorLog USE-INDEX TableName WHERE
                 ErrorLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN DO:

      IF icTableName > "" THEN DO:
   
         IF icKeyValue > "" THEN 
            FIND LAST ErrorLog USE-INDEX TableName WHERE
                      ErrorLog.Brand     = gcBrand     AND
                      ErrorLog.TableName = icTableName AND
                      ErrorLog.KeyValue  = icKeyValue NO-LOCK NO-ERROR.      

         ELSE 
            FIND LAST ErrorLog USE-INDEX TableName WHERE
                      ErrorLog.Brand     = gcBrand     AND
                      ErrorLog.TableName = icTableName NO-LOCK NO-ERROR.
      END.                 

      ELSE IF icActionID > "" THEN 
         FIND LAST ErrorLog USE-INDEX ActionID WHERE
                   ErrorLog.Brand    = gcBrand AND
                   ErrorLog.ActionID = icActionID NO-LOCK NO-ERROR.
                               
      ELSE FIND LAST ErrorLog USE-INDEX ActionID WHERE
                     ErrorLog.Brand = gcBrand NO-LOCK NO-ERROR.
   END.      

   ELSE IF order = 2 THEN DO:
      FIND LAST ErrorLog USE-INDEX TableName WHERE
                ErrorLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN DO:

      IF icTableName > "" THEN DO:
   
         IF icKeyValue > "" THEN 
            FIND NEXT ErrorLog USE-INDEX TableName WHERE
                      ErrorLog.Brand     = gcBrand     AND
                      ErrorLog.TableName = icTableName AND
                      ErrorLog.KeyValue  = icKeyValue NO-LOCK NO-ERROR.      

         ELSE 
            FIND NEXT ErrorLog USE-INDEX TableName WHERE
                      ErrorLog.Brand     = gcBrand     AND
                      ErrorLog.TableName = icTableName NO-LOCK NO-ERROR.
      END.                 

      ELSE IF icActionID > "" THEN 
         FIND NEXT ErrorLog USE-INDEX ActionID WHERE
                   ErrorLog.Brand    = gcBrand AND
                   ErrorLog.ActionID = icActionID NO-LOCK NO-ERROR.
                               
      ELSE FIND NEXT ErrorLog USE-INDEX ActionID WHERE
                     ErrorLog.Brand = gcBrand NO-LOCK NO-ERROR.
   END.      
 
   ELSE IF order = 2 THEN DO:
      FIND NEXT ErrorLog USE-INDEX TableName WHERE
                ErrorLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN DO:

      IF icTableName > "" THEN DO:
   
         IF icKeyValue > "" THEN 
            FIND PREV ErrorLog USE-INDEX TableName WHERE
                      ErrorLog.Brand     = gcBrand     AND
                      ErrorLog.TableName = icTableName AND
                      ErrorLog.KeyValue  = icKeyValue NO-LOCK NO-ERROR.      

         ELSE 
            FIND PREV ErrorLog USE-INDEX TableName WHERE
                      ErrorLog.Brand     = gcBrand     AND
                      ErrorLog.TableName = icTableName NO-LOCK NO-ERROR.
      END.                 

      ELSE IF icActionID > "" THEN 
         FIND PREV ErrorLog USE-INDEX ActionID WHERE
                   ErrorLog.Brand    = gcBrand AND
                   ErrorLog.ActionID = icActionID NO-LOCK NO-ERROR.
                               
      ELSE FIND PREV ErrorLog USE-INDEX ActionID WHERE
                     ErrorLog.Brand = gcBrand NO-LOCK NO-ERROR.
   END.      

   ELSE IF order = 2 THEN DO:
      FIND PREV ErrorLog USE-INDEX TableName WHERE
                ErrorLog.Brand = gcBrand  NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       ErrorLog.ActionID 
       ErrorLog.TableName 
       ErrorLog.KeyValue
       ldtDate
       ErrorLog.ErrorMsg
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

    fSplitTS(ErrorLog.ActionTS,
             OUTPUT ldtDate,
             OUTPUT liTime).


END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      lcTime = STRING(ldtDate,"99-99-99") + " " + 
               STRING(liTime,"hh:mm:ss").
      
      DISP 
           ErrorLog.ActionID
           ErrorLog.TableName
           ErrorLog.KeyValue
           lcTime
           ErrorLog.ActionTS
           ErrorLog.UserCode
           ErrorLog.ErrorCode
           ErrorLog.ErrorChar
           ErrorLog.ErrorMsg
      WITH FRAME lis.

      PAUSE MESSAGE "Press ENTER". 

      LEAVE.
   END.
END PROCEDURE.

