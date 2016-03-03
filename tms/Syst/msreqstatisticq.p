/* -----------------------------------------------
  MODULE .......: MSREQSTATQUEUE.P
  FUNCTION .....: Updates MsReqStatistic table through MsReqStatisticQ 
  APPLICATION ..: TMS
  CREATED ......: 05/2008 anttis
  MODIFIED .....: 
  VERSION ......: XFERA
------------------------------------------------------ */

{Syst/commpaa.i}
{Func/heartbeat.i}

ASSIGN
   gcBrand = "1".

DEFINE VARIABLE lcTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaDate     AS DATE      NO-UNDO.
DEFINE VARIABLE liLoops     AS INTEGER   NO-UNDO.
DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
DEFINE VARIABLE liHandled   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liLoopGap   AS INTEGER   NO-UNDO INIT 10.

FORM 
   liLoopGap  COLUMN-LABEL "Loop Gap"
   liLoops    COLUMN-LABEL "Loops"
   liHandled  COLUMN-LABEL "Handled"
   ldaDate    COLUMN-LABEL "Date"
   lcTime     COLUMN-LABEL "Time"
WITH
   WIDTH 55 TITLE " YOIGO MsReqStatistic Queue " CENTERED ROW 4
FRAME frm.

DEFINE BUFFER bufQueue FOR MsReqStatisticQ.

DISP
   liLoops
   liHandled 
   TODAY @ ldaDate
   STRING(TIME,"HH:MM:SS") @ lcTime 
WITH FRAME frm.

MESSAGE "Not in use" VIEW-AS ALERT-BOX.


LOOP:
DO WHILE TRUE:

   FOR EACH MsReqStatisticQ NO-LOCK
   i = 1 TO 1000:
      
      IF i MOD 50 = 0 THEN
         PUT SCREEN ROW 1 COL 1 STRING(i).
   
      PUT SCREEN ROW 23 "Processing ......".
      
      FIND FIRST bufQueue WHERE
           RECID(bufQueue) = RECID(MsReqStatisticQ)
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      
      IF NOT LOCKED(bufQueue) AND NOT ERROR-STATUS:ERROR THEN DO:

         FIND FIRST MsReqStatistic WHERE
            MsReqStatistic.Brand     = "1" AND
            MsReqStatistic.ReqType   = MsReqStatisticQ.ReqType AND 
            MsReqStatistic.ReqStatus = MsReqStatisticQ.ReqStatus 
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

         IF NOT AVAIL MsReqStatistic THEN DO:
            IF LOCKED MsReqStatistic THEN NEXT.
            
            CREATE MsReqStatistic.
            ASSIGN
               MsReqStatistic.Brand = "1"
               MsReqStatistic.ReqType = MsReqStatisticQ.ReqType
               MsReqStatistic.ReqStatus = MsReqStatisticQ.ReqStatus
               MsReqStatistic.ReqStatusCount = MsReqStatisticQ.ReqStatUpdate.
            
         END.
         ELSE DO:
            MsReqStatistic.ReqStatusCount = MsReqStatistic.ReqStatusCount 
                                         + MsReqStatisticQ.ReqStatUpdate.
         END.
         
         DELETE MsReqStatisticQ.
         liHandled = liHandled + 1.
      
      END.
   END.
  
   liLoops = liLoops + 1.

   DISP
      liLoopGap
      liLoops
      liHandled
      TODAY @ ldaDate
      STRING(TIME,"HH:MM:SS") @ lcTime 
   WITH FRAME frm.

   PUT SCREEN ROW 23 "Hit F8 to QUIT ..".
   
   fKeepAlive("MSReqStatistic").

   READKEY PAUSE liLoopGap.
   HIDE MESSAGE NO-PAUSE.
  
   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN LEAVE LOOP.

END.

QUIT.
