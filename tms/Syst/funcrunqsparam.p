/* ----------------------------------------------------------------------
  MODULE .......: FuncRunQSParam.p
  TASK .........: UPDATEs table FuncRunQSParam
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 15.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'FuncRunQSParam'}
{eventval.i}
{timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFuncRunQSParam AS HANDLE NO-UNDO.
   lhFuncRunQSParam = BUFFER FuncRunQSParam:HANDLE.
   RUN StarEventInitialize(lhFuncRunQSParam).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhFuncRunQSParam).
   END.

END.

DEF INPUT PARAMETER iiFRQScheduleID AS INT  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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

DEF VAR lcStatus       AS CHAR NO-UNDO.
DEF VAR lcField        AS CHAR NO-UNDO. 
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR lcConfName     AS CHAR NO-UNDO.
DEF VAR lcStartTime    AS CHAR NO-UNDO.
DEF VAR lcEndTime      AS CHAR NO-UNDO.
DEF VAR liSeq          AS INT  NO-UNDO.
DEF VAR lcParamName    AS CHAR NO-UNDO.
DEF VAR lcParamValue   AS CHAR NO-UNDO.
DEF VAR liUpdateRow    AS INT  NO-UNDO INIT 4. 
DEF VAR lcQueueDesc    AS CHAR NO-UNDO.

FORM
    lcConfName   FORMAT "X(20)" COLUMN-LABEL "Configuration" 
    lcParamName  FORMAT "X(20)" COLUMN-LABEL "Parameter"
    lcParamValue FORMAT "X(20)" COLUMN-LABEL "Value"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " PARAMETERS FOR SCHEDULED QUEUE " + lcQueueDesc + " "
    FRAME sel.

FORM
    FuncRunQSParam.FRQueueID     COLON 20
       lcQueueDesc NO-LABEL FORMAT "X(30)" SKIP
    FuncRunQSParam.FRQScheduleID COLON 20
    FuncRunQSParam.FRQRowSeq        COLON 20
    FuncRunQSParam.FRConfigID      COLON 20
       lcConfName NO-LABEL FORMAT "X(30)" SKIP
    FuncRunQSParam.ParamSeq       COLON 20
       lcParamName NO-LABEL FORMAT "X(30)" SKIP
    FuncRunQSParam.ParamType     COLON 20 
    lcParamValue               COLON 20 
       LABEL "Value" FORMAT "X(50)" SKIP
WITH  OVERLAY ROW liUpdateRow centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM
   FuncRunQSParam.DateParam FORMAT "99-99-9999"
   WITH OVERLAY NO-BOX NO-LABELS ROW liUpdateRow + 7 COL 26 FRAME fDateValue. 
FORM
   FuncRunQSParam.DecParam FORMAT "->>>>>>9.99"
   WITH OVERLAY NO-BOX NO-LABELS ROW liUpdateRow + 7 COL 26 FRAME fDecValue. 
FORM
   FuncRunQSParam.CharParam FORMAT "X(50)"
   WITH OVERLAY NO-BOX NO-LABELS ROW liUpdateRow + 7 COL 26 FRAME fCharValue. 
FORM
   FuncRunQSParam.LogParam FORMAT " Yes/No"
   WITH OVERLAY NO-BOX NO-LABELS ROW liUpdateRow + 7 COL 26 FRAME fLogValue. 
FORM
   FuncRunQSParam.IntParam FORMAT "->>>>>>>>9"
   WITH OVERLAY NO-BOX NO-LABELS ROW liUpdateRow + 7 COL 26 FRAME fIntValue. 


FUNCTION fGetParamField RETURNS CHAR
   (icParamType AS CHAR):

   DEF VAR lcField AS CHAR NO-UNDO.
   DEF VAR lhField AS HANDLE NO-UNDO.
   
   CASE icParamType:
   WHEN "Date"      THEN lcField = "DateParam".
   WHEN "Decimal"   THEN lcField = "DecParam".
   WHEN "Character" THEN lcField = "CharParam".
   WHEN "Logical"   THEN lcField = "LogParam".
   WHEN "Integer"   THEN lcField = "IntParam".
   OTHERWISE lcField = "".
   END CASE. 

   IF lcField = "" THEN RETURN "".

   IF icParamType = "Date" THEN 
      RETURN STRING(lhFuncRunQSParam:BUFFER-FIELD(lcField):BUFFER-VALUE,
                    "99-99-9999").
   ELSE RETURN lhFuncRunQSParam:BUFFER-FIELD(lcField):BUFFER-VALUE.
   
END FUNCTION.

FIND FIRST FuncRunQSchedule WHERE 
   FuncRunQSchedule.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
IF NOT AVAILABLE FuncRunQSchedule THEN DO:
   MESSAGE "Scheduling data not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND FIRST FuncRunQueue WHERE 
   FuncRunQueue.FRQueueID = FuncRunQSchedule.FRQueueID 
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE FuncRunQueue THEN DO:
   MESSAGE "Queue not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
lcQueueDesc = FuncRunQueue.QueueDesc.

RUN pInitializeParams(iiFRQScheduleID).

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE FuncRunQSParam THEN ASSIGN
   Memory       = recid(FuncRunQSParam)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No parameters available" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
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

        UP FRAME-LINE - 1.
        FIND FuncRunQSParam WHERE recid(FuncRunQSParam) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FuncRunQSParam THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FuncRunQSParam).
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
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW lcConfName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) lcConfName WITH FRAME sel.
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
        FIND FuncRunQSParam WHERE recid(FuncRunQSParam) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FuncRunQSParam THEN
              ASSIGN FIRSTrow = i Memory = recid(FuncRunQSParam).
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
           IF NOT AVAILABLE FuncRunQSParam THEN DO:
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
                rtab[1] = recid(FuncRunQSParam)
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
           IF NOT AVAILABLE FuncRunQSParam THEN DO:
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
              rtab[FRAME-DOWN] = recid(FuncRunQSParam).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FuncRunQSParam WHERE recid(FuncRunQSParam) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FuncRunQSParam THEN DO:
           Memory = recid(FuncRunQSParam).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FuncRunQSParam THEN 
                 Memory = recid(FuncRunQSParam).
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
           FIND FuncRunQSParam WHERE recid(FuncRunQSParam) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFuncRunQSParam).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndTS */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFuncRunQSParam).

       RUN local-disp-row.
       xrecid = recid(FuncRunQSParam).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FuncRunQSParam) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FuncRunQSParam) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FuncRunQSParam WHERE recid(FuncRunQSParam) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FuncRunQSParam WHERE recid(FuncRunQSParam) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST FuncRunQSParam USE-INDEX FuncRunQSchedule WHERE 
      FuncRunQSParam.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST FuncRunQSParam USE-INDEX FuncRunQSchedule WHERE 
      FuncRunQSParam.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT FuncRunQSParam USE-INDEX FuncRunQSchedule WHERE 
      FuncRunQSParam.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV FuncRunQSParam USE-INDEX FuncRunQSchedule WHERE 
      FuncRunQSParam.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       lcConfName
       lcParamName
       lcParamValue
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   lcConfName = "".
   FIND FIRST FuncRunConfig WHERE 
      FuncRunConfig.FRConfigID = FuncRunQSParam.FRConfigID 
      NO-LOCK NO-ERROR.
   IF AVAILABLE FuncRunConfig THEN lcConfName = FuncRunConfig.ConfName.
   
   lcParamName = "".
   FIND FIRST FuncRunParam WHERE
              FuncRunParam.FRConfigID = FuncRunQSParam.FRConfigID AND
              FuncRunParam.ParamSeq   = FuncRunQSParam.ParamSeq 
      NO-LOCK NO-ERROR.
   IF AVAILABLE FuncRunParam THEN lcParamName = FuncRunParam.ParamName.
    
   lcParamValue = fGetParamField(FuncRunQSParam.ParamType).
    
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         FuncRunQSParam.FRQueueID
         lcQueueDesc
         FuncRunQSParam.FRQRowSeq
         FuncRunQSParam.FRQScheduleID        
         FuncRunQSParam.FRConfigID
         lcConfName
         FuncRunQSParam.ParamSeq
         lcParamName
         FuncRunQSParam.ParamType
         lcParamValue
      WITH FRAME lis.

      IF NOT NEW FuncRunQSParam THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
      END.
      ELSE toimi = 1.
      
      IF toimi = 1 THEN DO:

         UpdateField:
         REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
            FIND CURRENT FuncRunQSParam EXCLUSIVE-LOCK.
            ehto = 9.
            RUN ufkey.
         
            PAUSE 0.
            CASE FuncRunQSParam.ParamType:
            WHEN "Date"      THEN DO:
               UPDATE FuncRunQSParam.DateParam WITH FRAME fDateValue.
               HIDE FRAME fDateValue NO-PAUSE.
            END.
            WHEN "Decimal"   THEN DO:
               UPDATE FuncRunQSParam.DecParam WITH FRAME fDecValue.
               HIDE FRAME fDecValue NO-PAUSE.
            END.
            WHEN "Character" THEN DO:
               UPDATE FuncRunQSParam.CharParam WITH FRAME fCharValue.
               HIDE FRAME fCharValue NO-PAUSE.
            END.
            WHEN "Logical"   THEN DO:
               UPDATE FuncRunQSParam.LogParam WITH FRAME fLogValue.
               HIDE FRAME fLogValue NO-PAUSE.
            END.
            WHEN "Integer"   THEN DO:
               UPDATE FuncRunQSParam.IntParam WITH FRAME fIntValue.
               HIDE FRAME fIntValue NO-PAUSE.
            END.
            END CASE.   

            LEAVE UpdateField.
         END.

         IF NEW FuncRunQSParam THEN LEAVE.
         
      END.

      ELSE IF toimi = 8 THEN LEAVE. 
   END.
   
END PROCEDURE.

PROCEDURE pInitializeParams:

   DEF INPUT PARAMETER iiFRQScheduleID AS INT NO-UNDO.
   
   RUN funcrunqsparam_initialize.p (iiFRQScheduleID).
   
END PROCEDURE.


